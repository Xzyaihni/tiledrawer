#![allow(clippy::suspicious_else_formatting)]
#![allow(clippy::match_like_matches_macro)]

use std::{
    env,
    mem,
    slice,
    thread,
    process,
    rc::Rc,
    cell::{RefMut, RefCell},
    fmt::Display,
    path::Path,
    time::Duration,
    collections::VecDeque,
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
    UiEvent,
    UiComplex,
    UiAnimatableId,
    StretchMode,
    ElementId,
    ElementPrimitiveId,
    UiElementPrimitive,
    UiElementComplex,
    UiElementType,
    ListElementInfo,
    KeepAspect
};

use animator::Animatable;
pub use point::Point2;

use config::Config;

mod point;
mod animator;
mod config;
mod ui;


pub fn complain(message: impl Display) -> !
{
    println!("{message}");

    process::exit(1)
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Color
{
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8
}

impl From<Color> for SdlColor
{
    fn from(x: Color) -> Self
    {
        SdlColor::RGBA(x.r, x.g, x.b, x.a)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureId(usize);

struct HeldTexture
{
    size: Point2<usize>,
    access: TextureAccess,
    texture: Texture<'static>
}

pub struct Assets
{
    creator: TextureCreator<WindowContext>,
    // i despise the lifetime on the texture, this sdl wrapper is absolute CANCER
    textures: Vec<HeldTexture>
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
        let texture = unsafe{ self.texture_from_image(access, image) };

        self.push_held(texture)
    }

    fn push_held(&mut self, texture: HeldTexture) -> TextureId
    {
        let id = self.textures.len();

        self.textures.push(texture);

        TextureId(id)
    }

    pub fn update_texture<'a, 'b>(
        &'a mut self,
        id: TextureId,
        image: &'b Image
    ) -> &'a mut Texture<'static>
    {
        let texture = &self.textures[id.0];
        if image.size() != texture.size
        {
            let access = texture.access;
            self.textures[id.0] = unsafe{ self.texture_from_image(access, image) };

            return self.texture_mut(id);
        }

        let data = image.data_bytes();
        let row = image.bytes_row();

        let texture = self.texture_mut(id);
        texture.update(None, data, row).unwrap();

        texture
    }

    unsafe fn texture_from_image(&self, access: TextureAccess, image: &Image) -> HeldTexture
    {
        let size = image.size();
        let mut texture = self.creator.create_texture(
            PixelFormatEnum::RGBA32,
            access,
            size.x as u32,
            size.y as u32
        ).unwrap();

        texture.set_blend_mode(BlendMode::Blend);

        let data = image.data_bytes();

        texture.update(None, data, image.bytes_row()).unwrap();

        HeldTexture{
            size,
            access,
            texture: Self::make_texture_static(texture)
        }
    }

    unsafe fn make_texture_static(texture: Texture<'_>) -> Texture<'static>
    {
        mem::transmute(texture)
    }

    pub fn get_two_mut<'a>(
        &'a mut self,
        one: TextureId,
        two: TextureId
    ) -> (&'a mut Texture<'static>, &'a mut Texture<'static>)
    {
        let one = one.0;
        let two = two.0; 

        if one == two
        {
            panic!("get_two both indices cant be the same");
        }

        if one > two
        {
            let (left, right) = self.textures.split_at_mut(one);

            (&mut right[0].texture, &mut left[two].texture)
        } else
        {
            let (left, right) = self.textures.split_at_mut(two);

            (&mut left[one].texture, &mut right[0].texture)
        }
    }

    pub fn texture(&self, id: TextureId) -> &Texture<'static>
    {
        &self.textures[id.0].texture
    }

    pub fn texture_mut<'a>(&'a mut self, id: TextureId) -> &'a mut Texture<'static>
    {
        &mut self.textures[id.0].texture
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

struct Tool
{
    name: &'static str,
    id: ElementPrimitiveId
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
    pub selector_1d: ElementId,
    pub selector_1d_cursor: ElementId,
    pub draw_cursor: ElementId,
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

    pub fn needs_redraw(&self) -> bool
    {
        self.needs_redraw
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

struct Hsv
{
    pub h: f32,
    pub s: f32,
    pub v: f32
}

impl From<Hsv> for Color
{
    fn from(hsv: Hsv) -> Self
    {
        let t = |v: f32|
        {
            (v * 255.0) as u8
        };

        let f = |n: f32| -> u8
        {
            let k = (n + hsv.h / 60.0) % 6.0;

            t(hsv.v - hsv.v * hsv.s * (k.min(4.0 - k)).clamp(0.0, 1.0))
        };

        Color{r: f(5.0), g: f(3.0), b: f(1.0), a: 255}
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
    color_slider: f32,
    color_position: Point2<f32>,
    billinear: bool,
    mouse_position: Point2<i32>,
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
        let mut ui = Ui::new(window.clone(), assets.clone());

        let ui_group = Self::new_ui(&mut ui, &font, assets.clone(), draw_color, &image);

        let mut this = Self{
            window,
            assets,
            image: DrawImage::new(image),
            ui,
            ui_group,
            scale,
            color_slider: 0.0,
            color_position: Point2{x: 0.0, y: 0.0},
            billinear,
            mouse_position: Point2{x: 0, y: 0},
            draw_color,
            erase_color: Color{r: 0, g: 0, b: 0, a: 0},
            previous_draw: None,
            controls: Controls::new()
        };

        this.set_selected_color();
        this.update_cursor();
        this.update_1d_cursor();
        this.update_2d_cursor();

        this
    }

    fn new_ui(
        ui: &mut Ui,
        font: &Font<'_, 'static>,
        assets: Rc<RefCell<Assets>>,
        draw_color: Color,
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

        let selected_part = 0.13;
        let div = 0.9;

        let selector_2d = ui.push_child(&color_selector, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: selected_part},
            size: Point2{x: div - half_pad.x, y: 1.0 - selected_part - half_pad.y}.into(),
            texture: Some(selector_2d_texture)
        });

        let selector_1d = ui.push_child(&color_selector, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: div, y: selected_part},
            size: Point2{x: 1.0 - div, y: 1.0 - selected_part - half_pad.y}.into(),
            texture: Some(selector_1d_texture)
        });

        let color_image = Image::repeat(1, 1, draw_color);
        let selected_color = assets.borrow_mut().add_texture(&color_image);

        ui.push_child(&color_selector, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: half_pad.y},
            size: Point2{x: 1.0, y: selected_part - padding.y}.into(),
            texture: Some(selected_color)
        });

        let t = |c|
        {
            Self::texture_filled(assets.clone(), c)
        };

        let tools = {
            let names = ["brush", "pipette", "fill"];

            let items = names.iter().map(|name|
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

            let tools_list = ui.push_child(&side_screen, UiElementComplex::List(ListElementInfo{
                items,
                pos: Point2{x: 0.0, y: 0.0},
                size: Point2{x: 1.0, y: 1.0 - height}.into(),
                item_height: 0.2,
                background: t(Color{r: 242, g: 242, b: 242, a: 255}),
                scroll_background: t(Color{r: 235, g: 235, b: 235, a: 255}),
                scrollbar: t(Color{r: 205, g: 205, b: 205, a: 255})
            }));

            let tools_list = ui.get_complex(&tools_list);
            let tools_list = tools_list.borrow();

            let tools_list = match &*tools_list
            {
                UiComplex::List(x) => x,
                _ => unreachable!()
            };

            tools_list.children().frames_iter().zip(names).map(|(child, name)|
            {
                let id: &ElementPrimitiveId = (child).try_into().unwrap();

                let id = id.clone();

                Tool{
                    name,
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

        let element = UiElementPrimitive{
            size: KeepAspect::new(StretchMode::Min, Point2{x: 0.9, y: 0.9}).into(),
            ..element
        };

        let selector_1d_cursor = ui.push_child(&selector_1d, element);

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
            selector_1d_cursor,
            draw_cursor,
            tools
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
        output.pixels_mut().for_each(|(big_pos, color)|
        {
            let pos = big_pos.map(|a| a as f32) * self.scale;

            let new_color = if self.billinear
            {
                let pos = pos.zip(self.image.size()).map(|(a, b)| a % b as f32);

                self.image.get_billinear_overflowing(pos)
            } else
            {
                self.image[pos.zip(self.image.size()).map(|(a, b)| a as usize % b)]
            };

            // just arbitrary numbers kinda lol
            let t_pos = if self.scale > 1.0
            {
                big_pos
            } else if self.scale > 0.5
            {
                big_pos / 2
            } else if self.scale > 0.1
            {
                big_pos / 4
            } else
            {
                big_pos / 8
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

            let mixed_color = if new_color.a == 255
            {
                new_color
            } else
            {
                Image::lerp(transparency, new_color, new_color.a as f32 / 255.0)
            };

            *color = mixed_color;
        });
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

            *color = Self::select_color(self.color_slider, pos.x, pos.y);
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

    fn draw_ui_element<F>(&self, element: &ElementId, texture: TextureId, f: F)
    where
        F: FnOnce(&mut Image)
    {
        let size = self.ui.pixels_size(element).map(|x| x as usize);
        let mut main_surface = Image::repeat(size.x, size.y, Color{r: 0, g: 0, b: 0, a: 0});

        f(&mut main_surface);

        self.assets.borrow_mut().update_texture(texture, &main_surface);
    }

    pub fn draw(&mut self)
    {
        if self.image.needs_redraw()
        {
            self.draw_ui_element(&self.ui_group.main_screen, self.ui_group.main_texture, |image|
            {
                self.draw_main_image(image);
            });
        }

        self.draw_ui_element(
            &self.ui_group.selector_2d,
            self.ui_group.selector_2d_texture,
            |image|
            {
                self.draw_selector2d_image(image);
            });

        self.draw_ui_element(
            &self.ui_group.selector_1d,
            self.ui_group.selector_1d_texture,
            |image|
            {
                self.draw_selector1d_image(image);
            });

        let image = Image::repeat(1, 1, self.draw_color);
        self.assets.borrow_mut().update_texture(self.ui_group.selected_color, &image);

        self.ui.draw();
    }

    pub fn image(&self) -> &Image
    {
        &self.image
    }

    pub fn update(&mut self, dt: f32)
    {
        self.update_cursor();

        let speed = 1.0;
        if self.controls.is_down(Control::ZoomIn)
        {
            self.scale *= 1.0 - speed * dt;
        } else if self.controls.is_down(Control::ZoomOut)
        {
            self.scale *= 1.0 + speed * dt;
        }

        let draw_with = |this: &mut Self, color|
        {
            if let Some(position) = this.mouse_image()
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
            }
        };

        if self.controls.is_down(Control::Draw)
        {
            draw_with(self, self.draw_color);

            if let Some(position) = self.mouse_inside(&self.ui_group.selector_1d)
            {
                self.color_slider = position.y;
                self.set_selected_color();
                self.update_1d_cursor();
            } else if let Some(position) = self.mouse_inside(&self.ui_group.selector_2d)
            {
                self.color_position = position;
                self.set_selected_color();
                self.update_2d_cursor();
            }
        } else if self.controls.is_down(Control::Erase)
        {
            draw_with(self, self.erase_color);
        }
    }

    fn set_selected_color(&mut self)
    {
        let position = self.color_position;
        self.draw_color = Self::select_color(self.color_slider, position.x, position.y);
    }

    fn update_1d_cursor(&mut self)
    {
        let mut cursor = self.ui.get(&self.ui_group.selector_1d_cursor);

        cursor.set(UiAnimatableId::PositionCenteredX, 0.5);
        cursor.set(UiAnimatableId::PositionCenteredY, 1.0 - self.color_slider);
    }

    fn update_2d_cursor(&mut self)
    {
        let mut cursor = self.ui.get(&self.ui_group.selector_2d_cursor);

        let position = self.color_position;
        cursor.set(UiAnimatableId::PositionCenteredX, position.x);
        cursor.set(UiAnimatableId::PositionCenteredY, 1.0 - position.y);
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

    fn mouse_inside(&self, element: &ElementId) -> Option<Point2<f32>>
    {
        let mouse_position = self.mouse_normalized();

        self.ui.get(element)
            .borrow()
            .element()
            .inside_position(mouse_position)
            .map(|pos|
            {
                Point2{
                    y: 1.0 - pos.y,
                    ..pos
                }
            })
    }

    fn mouse_image(&self) -> Option<Point2<i32>>
    {
        let element = &self.ui_group.main_screen;

        let size = self.ui.pixels_size(element).map(|x| x as f32);
        self.mouse_inside(element).map(|pos|
        {
            (pos * size).map(|mouse|
            {
                (mouse * self.scale) as i32
            })
        })
    }

    fn on_control(&mut self, control: Control, state: State)
    {
        if control == Control::Draw
        {
            let event = self.ui.mouse_state(state.is_down(), self.mouse_normalized());
            if let Some(UiEvent{element_id: id}) = event
            {
                let tool = self.ui_group.tools.iter().find(|tool|
                {
                    tool.id == id
                });

                if let Some(tool) = tool
                {
                    dbg!(tool.name);
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
                match control
                {
                    Control::Draw | Control::Erase =>
                    {
                        self.previous_draw = None;
                    },
                    _ => ()
                }

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

        let x_lerp = point.x.fract();
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

        Self::lerp(bottom, top, point.y.fract())
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
