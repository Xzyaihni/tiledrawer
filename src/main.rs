use std::{
    env,
    mem,
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
    EventPump,
    mouse::MouseButton,
    keyboard::Keycode,
    render::{Canvas, Texture, TextureCreator, BlendMode},
    pixels::{PixelFormatEnum, Color},
    event::Event,
    video::{Window, WindowContext}
};

use image::{ImageError, RgbaImage, Rgba};
use ui::{Ui, ElementId, UiElement, UiElementType};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextureId(usize);

struct HeldTexture
{
    size: Point2<usize>,
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

    pub fn add_texture(&mut self, image: &Image) -> TextureId
    {
        let id = self.textures.len();

        let texture = unsafe{ self.texture_from_image(image) };
        self.textures.push(texture);

        TextureId(id)
    }

    pub fn update_texture(&mut self, id: TextureId, image: &Image)
    {
        if image.size() != self.textures[id.0].size
        {
            self.textures[id.0] = unsafe{ self.texture_from_image(image) };

            return;
        }

        let data = image.data_bytes();
        let row = image.bytes_row();

        self.texture_mut(id).update(None, &data, row).unwrap();
    }

    unsafe fn texture_from_image(&self, image: &Image) -> HeldTexture
    {
        let size = image.size();
        let mut texture = self.creator.create_texture_static(
            PixelFormatEnum::RGBA32,
            size.x as u32,
            size.y as u32
        ).unwrap();

        texture.set_blend_mode(BlendMode::Blend);

        let data = image.data_bytes();

        texture.update(None, &data, image.bytes_row()).unwrap();

        HeldTexture{
            size,
            texture: Self::make_texture_static(texture)
        }
    }

    unsafe fn make_texture_static(texture: Texture<'_>) -> Texture<'static>
    {
        mem::transmute(texture)
    }

    pub fn texture<'a>(&'a self, id: TextureId) -> &'a Texture<'static>
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

struct UiGroup
{
    pub main_texture: TextureId,
    pub main_screen: ElementId,
    pub selected_color: TextureId,
    pub selector_2d_texture: TextureId,
    pub selector_2d: ElementId,
    pub selector_1d_texture: TextureId,
    pub selector_1d: ElementId
}

const UNDO_LIMIT: usize = 20;

struct DrawImage
{
    image: Image,
    // pretty inefficient i guess, but its easier that way
    undos: VecDeque<Image>
}

impl DrawImage
{
    pub fn new(image: Image) -> Self
    {
        let undos = vec![image.clone()].into();

        Self{image, undos}
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

    pub fn set_down(&mut self, key: ControlRaw)
    {
        if let Some(control) = Self::key_to_control(key)
        {
            *self.control_mut(control) = State::Pressed;
        }
    }

    pub fn set_up(&mut self, key: ControlRaw)
    {
        if let Some(control) = Self::key_to_control(key)
        {
            *self.control_mut(control) = State::Released;
        }
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

struct DrawerWindow
{
    events: EventPump,
    window: Rc<RefCell<WindowWrapper>>,
    assets: Rc<RefCell<Assets>>,
    image: DrawImage,
    ui: Ui,
    ui_group: UiGroup,
    scale: f32,
    color_slider: f32,
    billinear: bool,
    mouse_position: Point2<i32>,
    draw_color: Color,
    erase_color: Color,
    previous_draw: Option<Point2<i32>>,
    controls: Controls
}

impl DrawerWindow
{
    pub fn new(image: Image, billinear: bool) -> Self
    {
        let ctx = sdl2::init().unwrap();

        let video = ctx.video().unwrap();

        let scale: f32 = 0.25;
        let scale_r = scale.recip().round() as u32;

        let width = image.width as u32 * 7 * scale_r;
        let height = image.height as u32 * 5 * scale_r;

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

        let main_texture = assets.borrow_mut().add_texture(&image);

        let events = ctx.event_pump().unwrap();

        let mut ui = Ui::new(window.clone(), assets.clone());

        let pos = Point2{x: 0.3, y: 0.0};
        let element = UiElement{
            kind: UiElementType::Panel,
            pos,
            size: Point2{x: 1.0 - pos.x, y: 1.0},
            texture: Some(main_texture)
        };

        let main_screen = ui.push(element);

        let color_image = Image::repeat(1, 1, Color{r: 0, g: 0, b: 0, a: 255});
        let selector_2d_texture = assets.borrow_mut().add_texture(&color_image);
        let selector_1d_texture = assets.borrow_mut().add_texture(&color_image);

        let height = 0.4;
        let element = UiElement{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: 1.0 - height},
            size: Point2{x: pos.x, y: height},
            texture: None
        };

        let color_selector = ui.push(element);

        let padding = Point2{
            x: 0.02,
            y: 0.02
        };

        let half_pad = padding / 2.0;

        let selected_part = 0.1;
        let div = 0.9;
        let element = UiElement{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: selected_part + half_pad.y},
            size: Point2{x: div - half_pad.y, y: 1.0 - selected_part - half_pad.y},
            texture: Some(selector_2d_texture)
        };

        let selector_2d = ui.push_child(&color_selector, element);

        let element = UiElement{
            kind: UiElementType::Panel,
            pos: Point2{x: div + half_pad.x, y: selected_part + half_pad.y},
            size: Point2{x: 1.0 - div - half_pad.y, y: 1.0 - selected_part - half_pad.y},
            texture: Some(selector_1d_texture)
        };

        let selector_1d = ui.push_child(&color_selector, element);

        let draw_color = Color{r: 0, g: 0, b: 0, a: 255};
        let color_image = Image::repeat(1, 1, draw_color);
        let selected_color = assets.borrow_mut().add_texture(&color_image);

        let element = UiElement{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: 0.0},
            size: Point2{x: 1.0, y: selected_part - half_pad.y},
            texture: Some(selected_color)
        };

        ui.push_child(&color_selector, element);

        let ui_group = UiGroup{
            main_texture,
            main_screen,
            selected_color,
            selector_2d_texture,
            selector_2d,
            selector_1d_texture,
            selector_1d
        };

        Self{
            events,
            window,
            assets,
            image: DrawImage::new(image),
            ui,
            ui_group,
            scale,
            color_slider: 0.0,
            billinear,
            mouse_position: Point2{x: 0, y: 0},
            draw_color,
            erase_color: Color{r: 0, g: 0, b: 0, a: 0},
            previous_draw: None,
            controls: Controls::new()
        }
    }

    pub fn canvas_mut(&mut self) -> RefMut<Canvas<Window>>
    {
        RefMut::map(self.window.borrow_mut(), |w| &mut w.canvas)
    }

    fn draw_main_image(&self, output: &mut Image)
    {
        output.pixels_mut().for_each(|(big_pos, color)|
        {
            let pos = big_pos.map(|a|
            {
                a as f32 * self.scale
            });

            let new_color = if self.billinear
            {
                let pos = pos.zip(self.image.size()).map(|(a, b)| a % b as f32);

                self.image.get_billinear_overflowing(pos)
            } else
            {
                self.image[pos.zip(self.image.size()).map(|(a, b)| a.round() as usize % b)]
            };

            let transparency = if (big_pos.x + big_pos.y % 2) % 2 == 0
            {
                Color{r: 255, g: 255, b: 255, a: 255}
            } else
            {
                Color{r: 0, g: 0, b: 0, a: 255}
            };

            *color = Image::lerp(transparency, new_color, new_color.a as f32 / 255.0);
        });
    }

    fn select_color(x: f32, y: f32, z: f32) -> Color
    {
        let f = |a|
        {
            (a * 255.0) as u8
        };

        Color{r: f(x), g: f(y), b: f(z), a: 255}
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

            *color = Self::select_color(pos.y, 1.0, 1.0);
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
        self.draw_ui_element(&self.ui_group.main_screen, self.ui_group.main_texture, |image|
        {
            self.draw_main_image(image);
        });

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
            } else if let Some(position) = self.mouse_inside(&self.ui_group.selector_2d)
            {
                self.draw_color = Self::select_color(self.color_slider, position.x, position.y);
            }
        } else if self.controls.is_down(Control::Erase)
        {
            draw_with(self, self.erase_color);
        }
    }

    fn mouse_inside(&self, element: &ElementId) -> Option<Point2<f32>>
    {
        let window_size = self.window.borrow().window_size().map(|x| x as f32);
        let mouse_position = self.mouse_position.map(|x| x as f32) / window_size;

        let mouse_position = Point2{
            y: 1.0 - mouse_position.y,
            ..mouse_position
        };

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
                (mouse as f32 * self.scale).round() as i32
            })
        })
    }

    pub fn wait_exit(&mut self)
    {
        let fps = 60;
        let dt = 1.0 / fps as f32;

        loop
        {
            let mut on_down = |control|
            {
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
            };

            let mut on_up = |control|
            {
                match control
                {
                    Control::Draw | Control::Erase =>
                    {
                        self.previous_draw = None;
                    },
                    _ => ()
                }
            };

            for event in self.events.poll_iter()
            {
                match event
                {
                    Event::Quit{..} => return,
                    Event::KeyDown{keycode: Some(key), ..} =>
                    {
                        let key = ControlRaw::Keyboard(key);

                        if let Some(key) = Controls::key_to_control(key.clone())
                        {
                            on_down(key);
                        }

                        self.controls.set_down(key);
                    },
                    Event::KeyUp{keycode: Some(key), ..} =>
                    {
                        let key = ControlRaw::Keyboard(key);
                        if let Some(key) = Controls::key_to_control(key.clone())
                        {
                            on_up(key);
                        }

                        self.controls.set_up(key);
                    },
                    Event::MouseButtonDown{mouse_btn: button, ..} =>
                    {
                        let key = ControlRaw::Mouse(button);
                        if let Some(key) = Controls::key_to_control(key.clone())
                        {
                            on_down(key);
                        }

                        self.controls.set_down(key);
                    },
                    Event::MouseButtonUp{mouse_btn: button, ..} =>
                    {
                        let key = ControlRaw::Mouse(button);
                        if let Some(key) = Controls::key_to_control(key.clone())
                        {
                            on_up(key);
                        }

                        self.controls.set_up(key);
                    },
                    Event::MouseMotion{x, y, ..} =>
                    {
                        self.mouse_position = Point2{x, y};
                    },
                    _ => ()
                }
            }

            self.update(dt);

            self.canvas_mut().clear();

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

    pub fn data_bytes(&self) -> Vec<u8>
    {
        self.data.iter().flat_map(|p|
        {
            [p.r, p.g, p.b, p.a]
        }).collect()
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

    let mut window = DrawerWindow::new(image, config.billinear);

    window.wait_exit();

    window.image().save(config.output).expect("saving must work");
}
