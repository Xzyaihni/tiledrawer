#![allow(clippy::suspicious_else_formatting)]
#![allow(clippy::match_like_matches_macro)]

use std::{
    env,
    mem,
    slice,
    thread,
    process,
    rc::Rc,
    cell::{Ref, RefMut, RefCell},
    fmt::Display,
    path::Path,
    time::Duration,
    collections::{HashSet, VecDeque},
    ops::{Index, IndexMut, Deref, DerefMut}
};

use sdl2::{
    Sdl,
    EventPump,
    mouse::MouseButton,
    keyboard::Keycode,
    rect::Rect,
    surface::Surface,
    ttf::{Sdl2TtfContext, Font},
    render::{Canvas, Texture, TextureCreator, TextureAccess, BlendMode},
    pixels::{PixelFormatEnum, Color as SdlColor},
    event::{WindowEvent, Event},
    video::{Window, WindowContext}
};

use image::{ImageError, RgbaImage, Rgba};

use ui::{
    Ui,
    UiComplex,
    UiAnimatableId,
    UiElementGlobal,
    StretchMode,
    ElementId,
    ElementPrimitiveId,
    UiElementPrimitive,
    UiElementComplex,
    UiElementType,
    ButtonTextures,
    ScrollElementInfo,
    ScrollTextures,
    ListElementInfo,
    KeepAspect
};

use animator::Animatable;
pub use point::Point2;

pub use color::Color;
use color::{Hsv, Hsva};

use config::Config;

mod point;
mod animator;
mod color;
mod config;
mod ui;


pub fn complain(message: impl Display) -> !
{
    println!("{message}");

    process::exit(1)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureId(usize);

struct HeldTexture
{
    size: Point2<usize>,
    access: TextureAccess,
    // i despise the lifetime on the texture, this sdl wrapper is absolute CANCER
    texture: Texture<'static>
}

pub struct Assets
{
    creator: TextureCreator<WindowContext>,
    textures: Vec<RefCell<HeldTexture>>
}

impl Assets
{
    pub fn new(creator: TextureCreator<WindowContext>) -> Self
    {
        Self{
            creator,
            textures: Vec::new()
        }
    }

    pub fn add_texture_from_surface(&mut self, surface: Surface) -> TextureId
    {
        let size = surface.size();

        let texture = surface.as_texture(&self.creator).unwrap();
        let texture = unsafe{ Self::make_texture_static(texture) };

        let held = HeldTexture{
            access: TextureAccess::Static,
            size: Point2{x: size.0 as usize, y: size.1 as usize},
            texture
        };

        self.push_held(held)
    }

    pub fn add_texture(&mut self, image: &Image) -> TextureId
    {
        self.add_texture_access(TextureAccess::Static, image)
    }

    pub fn add_texture_access(&mut self, access: TextureAccess, image: &Image) -> TextureId
    {
        let texture = unsafe{ self.texture_from_image(access, image) }.unwrap();

        self.push_held(texture)
    }

    fn push_held(&mut self, texture: HeldTexture) -> TextureId
    {
        let id = self.textures.len();

        self.textures.push(RefCell::new(texture));

        TextureId(id)
    }

    pub fn update_texture<'a, 'b>(
        &'a self,
        id: TextureId,
        image: &'b Image
    ) -> RefMut<'a, Texture<'static>>
    {
        let texture = &self.textures[id.0];
        let mut texture = texture.borrow_mut();
        if image.size() != texture.size
        {
            let access = texture.access;

            if let Some(new_texture) = unsafe{ self.texture_from_image(access, image) }
            {
                *texture = new_texture;
            }

            return RefMut::map(texture, |c| &mut c.texture);
        }

        let data = image.data_bytes();
        let row = image.bytes_row();

        texture.texture.update(None, data, row).unwrap();

        RefMut::map(texture, |c| &mut c.texture)
    }

    // cant do anything about the errors anyway, just Option it
    unsafe fn texture_from_image(
        &self,
        access: TextureAccess,
        image: &Image
    ) -> Option<HeldTexture>
    {
        let size = image.size();
        let mut texture = self.creator.create_texture(
            PixelFormatEnum::RGBA32,
            access,
            size.x as u32,
            size.y as u32
        ).ok()?;

        texture.set_blend_mode(BlendMode::Blend);

        let data = image.data_bytes();

        texture.update(None, data, image.bytes_row()).ok()?;

        Some(HeldTexture{
            size,
            access,
            texture: Self::make_texture_static(texture)
        })
    }

    unsafe fn make_texture_static(texture: Texture<'_>) -> Texture<'static>
    {
        mem::transmute(texture)
    }

    pub fn texture(&self, id: TextureId) -> Ref<Texture<'static>>
    {
        Ref::map(self.textures[id.0].borrow(), |c| &c.texture)
    }

    pub fn texture_mut<'a>(&'a self, id: TextureId) -> RefMut<'a, Texture<'static>>
    {
        RefMut::map(self.textures[id.0].borrow_mut(), |c| &mut c.texture)
    }
}

pub struct WindowWrapper
{
    pub canvas: Canvas<Window>
}

impl WindowWrapper
{
    pub fn new(window: Window) -> Self
    {
        Self{
            canvas: window.into_canvas().present_vsync().build().unwrap()
        }
    }

    pub fn window_size(&self) -> Point2<u32>
    {
        let (width, height) = self.canvas.output_size().unwrap();

        Point2{x: width, y: height}
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ToolId
{
    Pipette,
    Brush,
    Fill
}

#[allow(dead_code)]
struct Tool
{
    name: &'static str,
    tool_id: ToolId,
    id: ElementPrimitiveId
}

struct ScrollWrapper
{
    id: ElementId,
    value: Rc<RefCell<UiComplex>>
}

impl ScrollWrapper
{
    pub fn new(ui: &Ui, id: ElementId) -> Self
    {
        let value = ui.get_complex(&id).clone();

        Self{id, value}
    }

    pub fn id(&self) -> &ElementId
    {
        &self.id
    }

    pub fn set(&mut self, value: f32)
    {
        let mut scroll = self.value.borrow_mut();

        match &mut *scroll
        {
            UiComplex::Scroll(x) =>
            {
                x.set_scroll(value);
            },
            _ => unreachable!()
        }
    }

    pub fn get(&mut self) -> f32
    {
        let scroll = self.value.borrow();

        match &*scroll
        {
            UiComplex::Scroll(x) => x.scroll(),
            _ => unreachable!()
        }
    }
}

struct UiGroup
{
    pub main_texture: TextureId,
    pub main_screen: ElementId,
    pub selected_color: TextureId,
    pub selector_2d_texture: TextureId,
    pub selector_2d: ElementId,
    pub selector_2d_cursor: ElementId,
    pub selector_1d_texture: TextureId,
    pub selector_1d: ScrollWrapper,
    pub draw_cursor: ElementId,
    pub tools_list: Rc<RefCell<UiComplex>>,
    pub tools: Vec<Tool>
}

const UNDO_LIMIT: usize = 20;

struct DrawImage
{
    image: Image,
    needs_redraw: bool,
    // pretty inefficient i guess, but its easier that way
    undos: VecDeque<Image>
}

impl DrawImage
{
    pub fn new(image: Image) -> Self
    {
        let undos = vec![image.clone()].into();

        Self{image, needs_redraw: true, undos}
    }

    pub fn redraw(&mut self) -> bool
    {
        let needs = self.needs_redraw;

        self.needs_redraw = false;

        needs
    }

    pub fn add_undo(&mut self)
    {
        if self.undos.len() > UNDO_LIMIT
        {
            self.undos.pop_front();
        }

        self.undos.push_back(self.image.clone());
    }

    pub fn undo(&mut self)
    {
        if let Some(previous) = self.undos.pop_back()
        {
            self.image = previous;
        }

        self.needs_redraw = true;
    }
}

impl Deref for DrawImage
{
    type Target = Image;

    fn deref(&self) -> &Self::Target
    {
        &self.image
    }
}

impl DerefMut for DrawImage
{
    fn deref_mut(&mut self) -> &mut Self::Target
    {
        // just a guess
        self.needs_redraw = true;

        &mut self.image
    }
}

#[derive(Debug, Clone)]
enum ControlRaw
{
    Keyboard(Keycode),
    Mouse(MouseButton)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Control
{
    Draw = 0,
    Erase,
    ZoomOut,
    ZoomIn,
    Undo,
    ColorPick,
    Last
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State
{
    Pressed,
    Released
}

impl State
{
    pub fn is_down(&self) -> bool
    {
        match self
        {
            Self::Pressed => true,
            _ => false
        }
    }
}

struct Controls
{
    states: Vec<State>
}

impl Controls
{
    pub fn new() -> Self
    {
        Self{states: vec![State::Released; Control::Last as usize]}
    }

    pub fn is_down(&self, control: Control) -> bool
    {
        match self.control(control)
        {
            State::Pressed => true,
            _ => false
        }
    }

    #[allow(dead_code)]
    pub fn set_down(&mut self, key: ControlRaw)
    {
        if let Some(control) = Self::key_to_control(key)
        {
            self.set_control_down(control);
        }
    }

    #[allow(dead_code)]
    pub fn set_up(&mut self, key: ControlRaw)
    {
        if let Some(control) = Self::key_to_control(key)
        {
            self.set_control_up(control);
        }
    }

    pub fn set_control_down(&mut self, control: Control)
    {
        *self.control_mut(control) = State::Pressed;
    }

    pub fn set_control_up(&mut self, control: Control)
    {
        *self.control_mut(control) = State::Released;
    }

    fn key_to_control(key: ControlRaw) -> Option<Control>
    {
        match key
        {
            ControlRaw::Keyboard(Keycode::Equals) => Some(Control::ZoomIn),
            ControlRaw::Keyboard(Keycode::Minus) => Some(Control::ZoomOut),
            ControlRaw::Keyboard(Keycode::Z) => Some(Control::Undo),
            ControlRaw::Keyboard(Keycode::LCtrl) => Some(Control::ColorPick),
            ControlRaw::Mouse(MouseButton::Left) => Some(Control::Draw),
            // my right mouse button is broken lmao
            ControlRaw::Mouse(MouseButton::Right)
                | ControlRaw::Keyboard(Keycode::V) => Some(Control::Erase),
            _ => None
        }
    }

    fn control(&self, control: Control) -> &State
    {
        &self.states[control as usize]
    }

    fn control_mut(&mut self, control: Control) -> &mut State
    {
        &mut self.states[control as usize]
    }
}

struct LazyUpdater<T>
{
    needs_update: bool,
    current: T
}

impl<T> LazyUpdater<T>
{
    pub fn new(current: T) -> Self
    {
        Self{needs_update: true, current}
    }

    pub fn set(&mut self, new_current: T)
    where
        T: PartialEq
    {
        if self.current != new_current
        {
            self.needs_update = true;

            self.current = new_current;
        }
    }

    #[allow(dead_code)]
    pub fn needs_update(&self) -> bool
    {
        self.needs_update
    }

    pub fn update(&mut self) -> bool
    {
        let previous = self.needs_update;

        self.needs_update = false;

        previous
    }
}

impl<T> Deref for LazyUpdater<T>
{
    type Target = T;

    fn deref(&self) -> &Self::Target
    {
        &self.current
    }
}

enum HeldState
{
    None,
    Draw,
    Erase
}

impl HeldState
{
    pub fn active(&self) -> bool
    {
        match self
        {
            Self::None => false,
            _ => true
        }
    }
}

struct DrawerWindow
{
    window: Rc<RefCell<WindowWrapper>>,
    assets: Rc<RefCell<Assets>>,
    image: DrawImage,
    ui: Ui,
    ui_group: UiGroup,
    scale: f32,
    color_slider: LazyUpdater<f32>,
    color_position: LazyUpdater<Point2<f32>>,
    billinear: bool,
    mouse_position: Point2<i32>,
    active_tool: ToolId,
    draw_held: HeldState,
    selector_2d_held: bool,
    draw_color: Color,
    erase_color: Color,
    previous_draw: Option<Point2<i32>>,
    controls: Controls
}

impl DrawerWindow
{
    pub fn new(
        ctx: Sdl,
        ttf_ctx: &Sdl2TtfContext,
        image: Image,
        billinear: bool
    ) -> Self
    {
        let video = ctx.video().unwrap();

        let font = ttf_ctx.load_font("font/OpenSans-Regular.ttf", 70).unwrap();

        let scale: f32 = 0.25;

        let width = 900;
        let height = 500;

        let window = video
            .window("cool drawer", width, height)
            .resizable()
            .build()
            .unwrap();

        let window = Rc::new(RefCell::new(WindowWrapper::new(window)));

        let assets = {
            let window = window.borrow();

            Rc::new(RefCell::new(Assets::new(window.canvas.texture_creator())))
        };

        let draw_color = Color{r: 0, g: 0, b: 0, a: 255};

        let active_tool = ToolId::Brush;

        let mut ui = Ui::new(window.clone(), assets.clone());

        let ui_group = Self::new_ui(
            &mut ui,
            &font,
            assets.clone(),
            draw_color,
            active_tool,
            &image
        );

        let mut this = Self{
            window,
            assets,
            image: DrawImage::new(image),
            ui,
            ui_group,
            scale,
            color_slider: LazyUpdater::new(0.0),
            color_position: LazyUpdater::new(Point2{x: 0.0, y: 0.0}),
            billinear,
            mouse_position: Point2{x: 0, y: 0},
            active_tool,
            draw_held: HeldState::None,
            selector_2d_held: false,
            draw_color,
            erase_color: Color{r: 0, g: 0, b: 0, a: 0},
            previous_draw: None,
            controls: Controls::new()
        };

        this.set_selected_color();
        this.update_cursor();
        this.update_color_selection(true);

        this
    }

    fn new_ui(
        ui: &mut Ui,
        font: &Font<'_, 'static>,
        assets: Rc<RefCell<Assets>>,
        draw_color: Color,
        active_tool: ToolId,
        image: &Image
    ) -> UiGroup
    {
        let main_texture = assets.borrow_mut().add_texture(image);

        let padding = Point2{
            x: 0.02,
            y: 0.02
        };

        let half_pad = padding / 2.0;

        let pos = Point2{x: 0.3, y: 0.0};
        let main_screen = ui.push(UiElementPrimitive{
            kind: UiElementType::Panel,
            pos,
            size: Point2{x: 1.0 - pos.x, y: 1.0}.into(),
            texture: Some(main_texture)
        });

        let side_screen = ui.push(UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: half_pad,
            size: Point2{x: pos.x - padding.x, y: 1.0 - padding.y}.into(),
            texture: None
        });

        let color_image = Image::repeat(1, 1, Color{r: 0, g: 0, b: 0, a: 255});
        let selector_2d_texture = assets.borrow_mut().add_texture(&color_image);
        let selector_1d_texture = assets.borrow_mut().add_texture(&color_image);

        let height = 0.4;
        let padding = Point2{
            x: 0.05,
            y: 0.05
        };

        let half_pad = padding / 2.0;

        let color_selector = ui.push_child(&side_screen, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: 1.0 - height},
            size: Point2{x: 1.0, y: height}.into(),
            texture: None
        });

        let t = |c, hover, pressed|
        {
            Self::create_button_texture(assets.clone(), c, hover, pressed)
        };

        let selected_part = 0.13;
        let div = 0.9;

        let selector_2d = ui.push_child(&color_selector, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: selected_part},
            size: Point2{x: div - half_pad.x, y: 1.0 - selected_part - half_pad.y}.into(),
            texture: Some(selector_2d_texture)
        });

        let selector_1d = ui.push_child(&color_selector, UiElementComplex::Scroll(ScrollElementInfo{
            pos: Point2{x: div, y: selected_part},
            size: Point2{x: 1.0 - div, y: 1.0 - selected_part - half_pad.y}.into(),
            bar_size: 0.1,
            textures: ScrollTextures{
                background: ButtonTextures::repeat(selector_1d_texture),
                scrollbar: t(Hsva{h: 0.0, s: 0.0, v: 1.0, a: 0.7}, 0.9, 0.8)
            }
        }));

        let selector_1d = ScrollWrapper::new(ui, selector_1d);

        let color_image = Image::repeat(1, 1, draw_color);
        let selected_color = assets.borrow_mut().add_texture(&color_image);

        ui.push_child(&color_selector, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: half_pad.y},
            size: Point2{x: 1.0, y: selected_part - padding.y}.into(),
            texture: Some(selected_color)
        });

        let output_tools_list;
        let tools = {
            let names = [
                (ToolId::Brush, "brush"),
                (ToolId::Pipette, "pipette"),
                (ToolId::Fill, "fill")
            ];

            let items = names.iter().map(|(_tool_id, name)|
            {
                let (rect, texture) = Self::create_text_texture(
                    assets.clone(),
                    font,
                    Color{r: 0, g: 0, b: 0, a: 255},
                    name
                );

                let (x, y) = rect.size();
                let size = Point2{x, y}.map(|x| x as f32);

                let aspect = size.y / size.x;

                let size = Point2{x: 1.0 / aspect, y: 1.0};

                UiElementPrimitive{
                    kind: UiElementType::Button,
                    pos: Point2{x: 0.0, y: 0.0},
                    size: KeepAspect::new(StretchMode::LockY, size).into(),
                    texture: Some(texture)
                }
            }).collect();

            let background = Self::texture_filled(
                assets.clone(),
                Hsv{h: 0.0, s: 0.0, v: 0.95}.into()
            );

            let tools_list = ui.push_child(&side_screen, UiElementComplex::List(ListElementInfo{
                items,
                pos: Point2{x: 0.0, y: 0.0},
                size: Point2{x: 1.0, y: 1.0 - height}.into(),
                item_height: 0.2,
                background,
                textures: t(Hsva{h: 0.0, s: 0.0, v: 0.95, a: 1.0}, 0.95, 0.9),
                scroll_textures: ScrollTextures{
                    background: t(Hsva{h: 0.0, s: 0.0, v: 0.92, a: 1.0}, 0.98, 0.92),
                    scrollbar: t(Hsva{h: 0.0, s: 0.0, v: 0.85, a: 1.0}, 0.9, 0.8)
                }
            }));

            let tools_list = ui.get_complex(&tools_list);

            output_tools_list = tools_list.clone();

            let mut tools_list = tools_list.borrow_mut();
            let tools_list = match &mut *tools_list
            {
                UiComplex::List(x) => x,
                _ => unreachable!()
            };

            let index = names.iter().position(|item| item.0 == active_tool).unwrap();
            tools_list.select_index(index);

            tools_list.children().frames_iter().zip(names).map(|(child, (tool_id, name))|
            {
                let id: &ElementPrimitiveId = (child).try_into().unwrap();

                let id = id.clone();

                Tool{
                    name,
                    tool_id,
                    id
                }
            }).collect()
        };

        let size = Point2::repeat(8);
        let square_cursor_texture = Self::procedural_texture(assets.clone(), size, |pos, pixel|
        {
            if pos.x == 0 || pos.x == (size.x - 1) || pos.y == 0 || pos.y == (size.y - 1)
            {
                *pixel = Color{r: 255, g: 255, b: 255, a: 255};
            }
        });

        let size = Point2::repeat(64);
        let thickness = 0.1;

        let circle_cursor_texture = Self::procedural_texture(assets.clone(), size, |pos, pixel|
        {
            let center = Point2{x: 0.5, y: 0.5};
            let pos = pos.map(|x| x as f32) / size.map(|x| x as f32);

            let pos = pos - center;

            let distance = pos.x.hypot(pos.y);

            let far = 0.5;
            let near = far - thickness;

            if distance > near && distance < far
            {
                *pixel = Color{r: 255, g: 255, b: 255, a: 255};
            }
        });

        let element = UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: 0.0},
            size: KeepAspect::new(StretchMode::Min, Point2{x: 0.06, y: 0.06}).into(),
            texture: Some(circle_cursor_texture)
        };

        let selector_2d_cursor = ui.push_child(&selector_2d, element.clone());

        let draw_cursor = ui.push_child(&main_screen, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: 0.0},
            size: KeepAspect::new(StretchMode::Min, Point2{x: 0.1, y: 0.1}).into(),
            texture: Some(square_cursor_texture)
        });

        UiGroup{
            main_texture,
            main_screen,
            selected_color,
            selector_2d_texture,
            selector_2d,
            selector_2d_cursor,
            selector_1d_texture,
            selector_1d,
            draw_cursor,
            tools_list: output_tools_list,
            tools
        }
    }

    fn create_button_texture(
        assets: Rc<RefCell<Assets>>,
        color: Hsva,
        hover: f32,
        pressed: f32
    ) -> ButtonTextures
    {
        let t = |c: Hsva|
        {
            Self::texture_filled(assets.clone(), c.into())
        };

        ButtonTextures{
            normal: t(color),
            hover: t(Hsva{v: color.v * hover, ..color}),
            pressed: t(Hsva{v: color.v * pressed, ..color})
        }
    }

    fn create_text_texture(
        assets: Rc<RefCell<Assets>>,
        font: &Font<'_, 'static>,
        color: Color,
        text: &str
    ) -> (Rect, TextureId)
    {
        let color: SdlColor = color.into();
        let surface = font.render(text).blended(color).unwrap();

        let rect = surface.rect();

        (rect, assets.borrow_mut().add_texture_from_surface(surface))
    }

    fn texture_filled(assets: Rc<RefCell<Assets>>, color: Color) -> TextureId
    {
        let image = Image::repeat(1, 1, color);

        assets.borrow_mut().add_texture(&image)
    }

    fn procedural_texture(
        assets: Rc<RefCell<Assets>>,
        size: Point2<usize>,
        f: impl Fn(Point2<usize>, &mut Color)
    ) -> TextureId
    {
        let empty_color = Color{r: 0, g: 0, b: 0, a: 0};

        let mut image = Image::repeat(size.x, size.y, empty_color);

        image.pixels_mut().for_each(|(pos, pixel)| f(pos, pixel));

        assets.borrow_mut().add_texture(&image)
    }

    pub fn canvas_mut(&mut self) -> RefMut<Canvas<Window>>
    {
        RefMut::map(self.window.borrow_mut(), |w| &mut w.canvas)
    }

    fn draw_main_image(&self, output: &mut Image)
    {
        output.pixels_mut().for_each(|(pixel_pos, color)|
        {
            let pos = self.position_to_image(pixel_pos.map(|x| x as f32));

            let new_color = if self.billinear
            {
                self.image.get_billinear_overflowing(pos)
            } else
            {
                self.image.get_overflowing(pos.map(|x| x as i32))
            };


            *color = self.with_transparency(pixel_pos, new_color);
        });
    }

    fn with_transparency(&self, pixel_pos: Point2<usize>, new_color: Color) -> Color
    {
        // just arbitrary numbers kinda lol
        let t_pos = if self.scale > 1.0
        {
            pixel_pos
        } else if self.scale > 0.5
        {
            pixel_pos / 2
        } else if self.scale > 0.1
        {
            pixel_pos / 3
        } else
        {
            pixel_pos / 4
        };

        let transparency = if (t_pos.x + t_pos.y % 2) % 2 == 0
        {
            let v = 255;

            Color{r: v, g: v, b: v, a: 255}
        } else
        {
            let v = 150;

            Color{r: v, g: v, b: v, a: 255}
        };

        if new_color.a == 255
        {
            new_color
        } else
        {
            Image::lerp(transparency, new_color, new_color.a as f32 / 255.0)
        }
    }

    fn select_color(x: f32, y: f32, z: f32) -> Color
    {
        Hsv{
            h: x * 360.0,
            s: y,
            v: 1.0 - z
        }.into()
    }

    fn draw_selector2d_image(&self, output: &mut Image)
    {
        let size_m = output.size().map(|x| (x - 1) as f32);
        output.pixels_mut().for_each(|(big_pos, color)|
        {
            let pos = big_pos.map(|x| x as f32) / size_m;

            *color = Self::select_color(*self.color_slider, pos.x, pos.y);
        });
    }

    fn draw_selector1d_image(&self, output: &mut Image)
    {
        let size_m = output.size().map(|x| (x - 1) as f32);
        output.pixels_mut().for_each(|(big_pos, color)|
        {
            let pos = big_pos.map(|x| x as f32) / size_m;

            *color = Self::select_color(pos.y, 1.0, 0.0);
        });
    }

    fn update_ui_texture<F>(&self, element: &ElementId, texture: TextureId, f: F)
    where
        F: FnOnce(&mut Image)
    {
        let size = self.ui.pixels_size(element).map(|x| x as usize);
        let mut main_surface = Image::repeat(size.x, size.y, Color{r: 0, g: 0, b: 0, a: 0});

        f(&mut main_surface);

        self.assets.borrow_mut().update_texture(texture, &main_surface);
    }

    fn update_color_selection(&mut self, do_set: bool)
    {
        self.color_slider.set(1.0 - self.ui_group.selector_1d.get());

        let mut cursor = self.ui.get(&self.ui_group.selector_2d_cursor);

        let position = *self.color_position;
        cursor.set(UiAnimatableId::PositionCenteredX, position.x);
        cursor.set(UiAnimatableId::PositionCenteredY, 1.0 - position.y);

        let updated_1d = self.color_slider.update();
        if updated_1d
        {
            self.update_ui_texture(
                self.ui_group.selector_1d.id(),
                self.ui_group.selector_1d_texture,
                |image|
                {
                    self.draw_selector1d_image(image)
                }
            );
        }

        if self.color_position.update() || updated_1d
        {
            if do_set
            {
                self.set_selected_color();
            }

            self.update_ui_texture(
                &self.ui_group.selector_2d,
                self.ui_group.selector_2d_texture,
                |image|
                {
                    self.draw_selector2d_image(image)
                }
            );
        }
    }

    pub fn draw(&mut self)
    {
        if self.image.redraw()
        {
            self.update_ui_texture(&self.ui_group.main_screen, self.ui_group.main_texture, |image|
            {
                self.draw_main_image(image);
            });
        }

        let image = Image::repeat(1, 1, self.draw_color);
        self.assets.borrow_mut().update_texture(self.ui_group.selected_color, &image);

        self.ui.draw();
    }

    pub fn image(&self) -> &Image
    {
        &self.image
    }

    fn handle_brush(&mut self, position: Point2<i32>)
    {
        let draw_with = |this: &mut Self, color|
        {
            if let Some(previous) = this.previous_draw
            {
                this.image.line_overflowing(previous, position, color);
            } else
            {
                let pos = this.image.overflowing_pos(position);
                this.image[pos] = color;
            }

            this.previous_draw = Some(position);
        };

        if let Some(color) = self.held_color()
        {
            draw_with(self, color);
        }
    }

    fn handle_pipette(&mut self, position: Point2<i32>)
    {
        let pos = self.image.overflowing_pos(position);

        self.set_sliders_to(self.image[pos]);
    }

    fn handle_fill(&mut self, position: Point2<i32>)
    {
        if let Some(color) = self.held_color()
        {
            let pos = self.image.overflowing_pos(position);

            self.image.flood_fill(pos, color);
        }
    }

    fn held_color(&self) -> Option<Color>
    {
        match self.draw_held
        {
            HeldState::None => None,
            HeldState::Draw => Some(self.draw_color),
            HeldState::Erase => Some(self.erase_color)
        }
    }

    pub fn update(&mut self, dt: f32)
    {
        self.update_cursor();
        self.update_color_selection(true);

        let speed = 1.0;
        if self.controls.is_down(Control::ZoomIn)
        {
            self.scale *= 1.0 - speed * dt;
        } else if self.controls.is_down(Control::ZoomOut)
        {
            self.scale *= 1.0 + speed * dt;
        }

        let draw_button = self.controls.is_down(Control::Draw);
        let erase_button = self.controls.is_down(Control::Erase);
        if !draw_button && !erase_button
        {
            self.draw_held = HeldState::None;
            self.previous_draw = None;
        }

        if !draw_button
        {
            self.selector_2d_held = false;
        }

        if self.selector_2d_held
        {
            let position = self.mouse_inside_saturating(&self.ui_group.selector_2d);

            self.color_position.set(position);
        }

        if self.draw_held.active()
        {
            if let Some(position) = self.mouse_image()
            {
                let tool = if self.controls.is_down(Control::ColorPick)
                {
                    ToolId::Pipette
                } else
                {
                    self.active_tool
                };

                match tool
                {
                    ToolId::Brush => self.handle_brush(position),
                    ToolId::Pipette => self.handle_pipette(position),
                    ToolId::Fill => self.handle_fill(position)
                }
            } else
            {
                self.previous_draw = None;
            }
        }
    }

    fn set_sliders_to(&mut self, color: Color)
    {
        let hsv = Hsv::from(color);
        self.ui_group.selector_1d.set(1.0 - hsv.h / 360.0);
        self.color_position.set(Point2{x: hsv.s, y: 1.0 - hsv.v});

        self.draw_color = color;

        self.update_color_selection(false);
    }

    fn set_selected_color(&mut self)
    {
        let position = *self.color_position;
        self.draw_color = Self::select_color(*self.color_slider, position.x, position.y);
    }

    fn update_cursor(&mut self)
    {
        let mut cursor = self.ui.get(&self.ui_group.draw_cursor);

        let total_size = self.ui.pixels_size(&self.ui_group.main_screen).map(|x| x as f32);
        let min_total_size = total_size.x.min(total_size.y);

        let single_pixel = min_total_size.recip() / self.scale;

        let cursor_size = single_pixel * 1.2;

        cursor.set(UiAnimatableId::ScaleX, cursor_size);
        cursor.set(UiAnimatableId::ScaleY, cursor_size);

        if let Some(pos) = self.mouse_image()
        {
            let pos = (pos + Point2{x: 0, y: 1}).map(|x| x as f32) / total_size / self.scale;

            let offset = single_pixel * 0.1;
            let pos = pos + Point2{x: -offset, y: offset};

            cursor.set(UiAnimatableId::PositionX, pos.x);
            cursor.set(UiAnimatableId::PositionY, 1.0 - pos.y);
        }
    }

    fn mouse_normalized(&self) -> Point2<f32>
    {
        let window_size = self.window.borrow().window_size().map(|x| x as f32);

        let mouse_position = self.mouse_position.map(|x| x as f32) / window_size;

        Point2{
            y: 1.0 - mouse_position.y,
            ..mouse_position
        }
    }

    fn is_mouse_inside(&self, element: &ElementId) -> bool
    {
        let mouse_position = self.mouse_normalized();

        self.ui.get(element).borrow().element().intersects(mouse_position)
    }

    fn mouse_inside(&self, element: &ElementId) -> Option<Point2<f32>>
    {
        let (m, out) = self.mouse_inside_with(|element, pos|
        {
            element.inside_position(pos)
        }, element);

        out.map(m)
    }

    fn mouse_inside_saturating(&self, element: &ElementId) -> Point2<f32>
    {
        let (m, out) = self.mouse_inside_with(|element, pos|
        {
            element.inside_position_saturating(pos)
        }, element);

        m(out)
    }

    fn mouse_inside_with<T, F>(
        &self,
        f: F,
        element: &ElementId
    ) -> (impl FnOnce(Point2<f32>) -> Point2<f32>, T)
    where
        F: FnOnce(&UiElementGlobal, Point2<f32>) -> T
    {
        let mouse_position = self.mouse_normalized();

        let m = |pos: Point2<f32>|
        {
            Point2{
                y: 1.0 - pos.y,
                ..pos
            }
        };

        let t = f(self.ui.get(element).borrow().element(), mouse_position);

        (m, t)
    }

    fn mouse_image(&self) -> Option<Point2<i32>>
    {
        let element = &self.ui_group.main_screen;

        let size = self.ui.pixels_size(element).map(|x| x as f32);
        self.mouse_inside(element).map(|pos|
        {
            self.position_to_image(pos * size).map(|x| x as i32)
        })
    }

    fn position_to_image(&self, position: Point2<f32>) -> Point2<f32>
    {
        position * self.scale
    }

    fn on_control(&mut self, control: Control, state: State)
    {
        self.ui.mouse_state(state.is_down(), self.mouse_normalized());

        let list = self.ui_group.tools_list.borrow();
        let list = match &*list
        {
            UiComplex::List(x) => x,
            _ => unreachable!()
        };

        if let Some(index) = list.selected()
        {
            self.active_tool = self.ui_group.tools[index].tool_id;
        }

        if state.is_down()
        {
            let draw_button = control == Control::Draw;
            let erase_button = control == Control::Erase;
            if draw_button || erase_button
            {
                if draw_button
                {
                    if self.is_mouse_inside(&self.ui_group.selector_2d)
                    {
                        self.selector_2d_held = true;
                    }
                }

                if self.is_mouse_inside(&self.ui_group.main_screen)
                {
                    self.draw_held = if draw_button
                    {
                        HeldState::Draw
                    } else
                    {
                        HeldState::Erase
                    };
                }
            }
        }

        match state
        {
            State::Pressed => {
                match control
                {
                    Control::Undo =>
                    {
                        self.image.undo();
                    },
                    Control::Draw | Control::Erase =>
                    {
                        self.image.add_undo();
                    },
                    _ => ()
                }

                self.controls.set_control_down(control);
            },
            State::Released =>
            {
                self.controls.set_control_up(control);
            }
        }
    }

    pub fn wait_exit(&mut self, events: &mut EventPump)
    {
        let fps = 60;
        let dt = 1.0 / fps as f32;

        loop
        {
            let mut special_event = false;

            for event in events.poll_iter()
            {
                match event
                {
                    Event::Quit{..} => return,
                    Event::KeyDown{keycode: Some(key), repeat: false, ..} =>
                    {
                        if let Some(key) = Controls::key_to_control(ControlRaw::Keyboard(key))
                        {
                            self.on_control(key, State::Pressed);
                        }
                    },
                    Event::KeyUp{keycode: Some(key), repeat: false, ..} =>
                    {
                        if let Some(key) = Controls::key_to_control(ControlRaw::Keyboard(key))
                        {
                            self.on_control(key, State::Released);
                        }
                    },
                    Event::MouseButtonDown{mouse_btn: button, ..} =>
                    {
                        if let Some(key) = Controls::key_to_control(ControlRaw::Mouse(button))
                        {
                            self.on_control(key, State::Pressed);
                        }
                    },
                    Event::MouseButtonUp{mouse_btn: button, ..} =>
                    {
                        if let Some(key) = Controls::key_to_control(ControlRaw::Mouse(button))
                        {
                            self.on_control(key, State::Released);
                        }
                    },
                    Event::MouseMotion{x, y, ..} =>
                    {
                        self.mouse_position = Point2{x, y};

                        self.ui.mouse_move(self.mouse_normalized());
                    },
                    Event::Window{win_event, ..} =>
                    {
                        match win_event
                        {
                            WindowEvent::SizeChanged(..) =>
                            {
                                self.ui.resized();

                                special_event = true;
                            },
                            WindowEvent::FocusGained
                                | WindowEvent::Exposed =>
                            {
                                special_event = true;
                            },
                            _ => ()
                        }
                    },
                    _ => ()
                }
            }

            // i hate the borrow checker
            if special_event
            {
                // self.needs_redraw = true;
            }

            self.update(dt);

            {
                let mut canvas = self.canvas_mut();

                canvas.set_draw_color(SdlColor{r: 255, g: 255, b: 255, a: 255});
                canvas.clear();
            }

            self.draw();

            self.canvas_mut().present();

            thread::sleep(Duration::from_millis(1000 / fps));
        }
    }
}

// values per pixel
const VPP: usize = 4;
// bytes per value
const BPV: usize = 1;
// bytes per pixel
const BPP: usize = VPP * BPV;

#[derive(Debug, Clone)]
pub struct Image
{
    data: Vec<Color>,
    width: usize,
    height: usize
}

impl Image
{
    pub fn repeat(width: usize, height: usize, c: Color) -> Self
    {
        Self{
            data: vec![c; width * height],
            width,
            height
        }
    }

    pub fn save(&self, path: impl AsRef<Path>) -> Result<(), ImageError>
    {
        RgbaImage::from_fn(self.width as u32, self.height as u32, |x, y|
        {
            let rgba = self[Point2{x, y}.map(|x| x as usize)];

            Rgba([rgba.r, rgba.g, rgba.b, rgba.a])
        }).save(path)
    }

    pub fn flood_fill(&mut self, pos: Point2<usize>, color: Color)
    {
        let mut visited = HashSet::new();
        let mut to_visit = vec![pos.map(|x| x as i32)];

        let start_color = self[pos];

        while let Some(pos) = to_visit.pop()
        {
            self.fill_inner(
                &mut visited,
                &mut to_visit,
                pos,
                &|compare| compare == start_color,
                &mut |pixel| *pixel = color
            );
        }
    }

    fn fill_inner<AF, SF>(
        &mut self,
        visited: &mut HashSet<Point2<usize>>,
        to_visit: &mut Vec<Point2<i32>>,
        pos: Point2<i32>,
        accept: &AF,
        set: &mut SF
    )
    where
        AF: Fn(Color) -> bool,
        SF: FnMut(&mut Color)
    {
        let pos = self.overflowing_pos(pos);

        let proceed = visited.insert(pos);

        if !proceed
        {
            return;
        }

        let this = &mut self[pos];
        if !accept(*this)
        {
            return;
        }

        set(this);

        let pos = pos.map(|x| x as i32);
        to_visit.push(Point2{x: pos.x + 1, ..pos});
		to_visit.push(Point2{x: pos.x - 1, ..pos});
		to_visit.push(Point2{y: pos.y + 1, ..pos});
		to_visit.push(Point2{y: pos.y - 1, ..pos});
    }

    pub fn line_overflowing(&mut self, mut start: Point2<i32>, end: Point2<i32>, c: Color)
    {
        let change = (end - start).map(|x| x.abs());
        let change = Point2{
            y: -change.y,
            ..change
        };

        let slope: Point2<i32> = start.zip(end).map(|(s, e)|
        {
            if s < e
            {
                1
            } else
            {
                -1
            }
        });

        let mut error = change.x + change.y;

        loop
        {
            *self.get_overflowing_mut(start) = c;

            if start == end
            {
                break;
            }

            let e2 = error * 2;
            if e2 >= change.y
            {
                if start.x == end.x
                {
                    break;
                }

                error += change.y;

                start.x += slope.x;
            }

            if e2 <= change.x
            {
                if start.y == end.y
                {
                    break;
                }

                error += change.x;

                start.y += slope.y;
            }
        }
    }

    pub fn get_billinear_saturating(&self, point: Point2<f32>) -> Color
    {
        self.get_billinear_with(|p| self.get_saturating(p), point)
    }

    pub fn get_billinear_overflowing(&self, point: Point2<f32>) -> Color
    {
        self.get_billinear_with(|p| self.get_overflowing(p), point)
    }

    fn get_billinear_with<G>(&self, mut g: G, point: Point2<f32>) -> Color
    where
        G: FnMut(Point2<i32>) -> Color
    {
        let low = point.map(|x| x.floor() as i32);
        let high = point.map(|x| x.ceil() as i32);

        let fract_abs = |x: f32|
        {
            let x = x.fract();

            if x > 0.0
            {
                x
            } else
            {
                1.0 + x
            }
        };

        let x_lerp = fract_abs(point.x);
        let top = Self::lerp(
            g(Point2{x: low.x, y: high.y}),
            g(Point2{x: high.x, y: high.y}),
            x_lerp
        );

        let bottom = Self::lerp(
            g(Point2{x: low.x, y: low.y}),
            g(Point2{x: high.x, y: low.y}),
            x_lerp
        );

        Self::lerp(bottom, top, fract_abs(point.y))
    }

    fn lerp(a: Color, b: Color, t: f32) -> Color
    {
        let mix = |a: u8, b: u8, t: f32|
        {
            (a as f32 * (1.0 - t) + b as f32 * t).round() as u8
        };

        Color{
            r: mix(a.r, b.r, t),
            g: mix(a.g, b.g, t),
            b: mix(a.b, b.b, t),
            a: mix(a.a, b.a, t),
        }
    }

    pub fn data_bytes(&self) -> &[u8]
    {
        let s: &[Color] = &self.data;
        let s: *const Color = s.as_ptr();
        let s = s as *const u8;

        let len = self.data.len() * BPP;
        let bytes: &[u8] = unsafe{ slice::from_raw_parts(s, len) };

        bytes
    }

    pub fn bytes_row(&self) -> usize
    {
        self.width * BPP
    }

    pub fn size(&self) -> Point2<usize>
    {
        Point2{
            x: self.width,
            y: self.height
        }
    }

    #[allow(dead_code)]
    fn inbounds(&self, pos: Point2<usize>) -> bool
    {
        pos.x < self.width && pos.y < self.height
    }

    fn saturating_pos(&self, pos: Point2<i32>) -> Point2<usize>
    {
        pos.zip(self.size()).map(|(x, size)| x.clamp(0, size as i32 - 1) as usize)
    }

    fn overflowing_pos(&self, pos: Point2<i32>) -> Point2<usize>
    {
        pos.zip(self.size()).map(|(x, size)|
        {
            let x = x % size as i32;

            (if x < 0
            {
                size as i32 + x
            } else
            {
                x
            }) as usize
        })
    }

    pub fn get_saturating(&self, pos: Point2<i32>) -> Color
    {
        self[self.saturating_pos(pos)]
    }

    pub fn get_overflowing(&self, pos: Point2<i32>) -> Color
    {
        self[self.overflowing_pos(pos)]
    }

    pub fn get_saturating_mut(&mut self, pos: Point2<i32>) -> &mut Color
    {
        let pos = self.saturating_pos(pos);
        &mut self[pos]
    }

    pub fn get_overflowing_mut(&mut self, pos: Point2<i32>) -> &mut Color
    {
        let pos = self.overflowing_pos(pos);
        &mut self[pos]
    }

    #[allow(dead_code)]
    pub fn pixels(&self) -> impl Iterator<Item=(Point2<usize>, &Color)> + '_
    {
        self.data.iter().enumerate().map(|(index, c)|
        {
            (self.index_to_pos(index), c)
        })
    }

    #[allow(dead_code)]
    pub fn pixels_mut(&mut self) -> impl Iterator<Item=(Point2<usize>, &mut Color)> + '_
    {
        let width = self.width;
        self.data.iter_mut().enumerate().map(move |(index, c)|
        {
            (Self::index_to_pos_assoc(width, index), c)
        })
    }

    pub fn to_index(&self, pos: Point2<usize>) -> usize
    {
        Self::to_index_assoc(self.width, pos)
    }

    pub fn to_index_assoc(width: usize, pos: Point2<usize>) -> usize
    {
        pos.y * width + pos.x
    }

    #[allow(dead_code)]
    pub fn index_to_pos(&self, index: usize) -> Point2<usize>
    {
        Self::index_to_pos_assoc(self.width, index)
    }

    #[allow(dead_code)]
    pub fn index_to_pos_assoc(width: usize, index: usize) -> Point2<usize>
    {
        Point2{
            x: index % width,
            y: index / width
        }
    }
}

impl From<RgbaImage> for Image
{
    fn from(value: RgbaImage) -> Self
    {
        Self{
            width: value.width() as usize,
            height: value.height() as usize,
            data: value.pixels().copied().map(|Rgba([r, g, b, a])|
            {
                Color{r, g, b, a}
            }).collect()
        }
    }
}

impl Index<Point2<usize>> for Image
{
    type Output = Color;

    fn index(&self, index: Point2<usize>) -> &Self::Output
    {
        &self.data[self.to_index(index)]
    }
}

impl IndexMut<Point2<usize>> for Image
{
    fn index_mut(&mut self, index: Point2<usize>) -> &mut Self::Output
    {
        let index = self.to_index(index);

        &mut self.data[index]
    }
}

fn main()
{
    let config = Config::parse(env::args().skip(1));

    let image = if let Some(input) = config.input
    {
        image::open(input)
            .expect("image must be openable, silly")
            .into_rgba8()
    } else
    {
        RgbaImage::from_pixel(config.width as u32, config.height as u32, Rgba([0, 0, 0, 255]))
    };

    let image = Image::from(image);

    let ctx = sdl2::init().unwrap();

    // i hate the sdl rust api i hate the sdl rust api i hate the sdl rust api
    let mut events = ctx.event_pump().unwrap();

    let ttf_ctx = sdl2::ttf::init().unwrap();

    let mut window = DrawerWindow::new(ctx, &ttf_ctx, image, config.billinear);

    window.wait_exit(&mut events);

    window.image().save(config.output).expect("saving must work");
}
