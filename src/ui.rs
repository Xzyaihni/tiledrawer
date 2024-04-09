use std::{
    rc::{Weak, Rc},
    cell::{Ref, RefMut, RefCell},
    ops::{Range, ControlFlow, Index}
};

use sdl2::{render::{Texture, TextureAccess}, rect::Rect};

use crate::{Image, Color, Point2, WindowWrapper, Assets, TextureId, animator::Animatable};


#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ComplexId(usize);

#[derive(Debug, Clone, PartialEq)]
pub enum ElementId
{
    Primitive(ElementPrimitiveId),
    Complex(ComplexId)
}

impl<'a> TryFrom<&'a ElementId> for &'a ElementPrimitiveId
{
    type Error = ();

    fn try_from(id: &ElementId) -> Result<&ElementPrimitiveId, Self::Error>
    {
        match id
        {
            ElementId::Primitive(x) => Ok(x),
            _ => Err(())
        }
    }
}

impl TryFrom<&ElementId> for ComplexId
{
    type Error = ();

    fn try_from(id: &ElementId) -> Result<Self, Self::Error>
    {
        match id
        {
            ElementId::Complex(x) => Ok(*x),
            _ => Err(())
        }
    }
}

enum PrimitiveIdWrapper<'a>
{
    Ref(&'a ElementPrimitiveId),
    Cell(Ref<'a, ElementPrimitiveId>)
}

impl<'a> From<&'a ElementPrimitiveId> for PrimitiveIdWrapper<'a>
{
    fn from(x: &'a ElementPrimitiveId) -> Self
    {
        Self::Ref(x)
    }
}

impl<'a> From<Ref<'a, ElementPrimitiveId>> for PrimitiveIdWrapper<'a>
{
    fn from(x: Ref<'a, ElementPrimitiveId>) -> Self
    {
        Self::Cell(x)
    }
}

impl<'a> PrimitiveIdWrapper<'a>
{
    fn as_ref(&self) -> &ElementPrimitiveId
    {
        match self
        {
            Self::Ref(x) => x,
            Self::Cell(x) => &x
        }
    }

    fn as_owned(self) -> ElementPrimitiveId
    {
        match self
        {
            Self::Ref(x) => x.clone(),
            Self::Cell(x) => x.clone()
        }
    }
}

impl ElementId
{
    fn primitive_id<'a>(&'a self, ui: &'a Ui) -> PrimitiveIdWrapper<'a>
    {
        match self
        {
            Self::Primitive(x) => PrimitiveIdWrapper::Ref(x),
            Self::Complex(x) => PrimitiveIdWrapper::Cell(ui.complex_to_primitive(*x))
        }
    }
}

trait TopLevelAdder
{
    fn add(&self, ui: &mut Ui, element: UiElementPrimitive) -> ElementPrimitiveId;
}

struct TopLevelAdderNormal;

impl TopLevelAdder for TopLevelAdderNormal
{
    fn add(&self, ui: &mut Ui, element: UiElementPrimitive) -> ElementPrimitiveId
    {
        ui.push(element).primitive_id(ui).as_owned()
    }
}

struct TopLevelAdderChild<'a>
{
    parent: &'a ElementId,
}

impl<'a> TopLevelAdder for TopLevelAdderChild<'a>
{
    fn add(&self, ui: &mut Ui, element: UiElementPrimitive) -> ElementPrimitiveId
    {
        ui.push_child(self.parent, element).primitive_id(ui).as_owned()
    }
}

struct ElementAdder<'a, TopLevel>
{
    ui: &'a mut Ui,
    adder: TopLevel
}

impl<'a, T: TopLevelAdder> ElementAdder<'a, T>
{
    fn top_level(&mut self, element: UiElementPrimitive) -> ElementPrimitiveId
    {
        self.adder.add(self.ui, element)
    }

    fn child(
        &self,
        parent: &ElementPrimitiveId,
        element: impl Into<UiElementPrimitiveWithFlags>
    ) -> ElementPrimitiveId
    {
        element.into().new_child_inner(self.ui, parent)
    }
}

#[derive(Debug, Clone)]
pub struct ScrollElement
{
    held: bool,
    dragging: bool,
    scroll: f32,
    body: Rc<RefCell<UiPrimitive>>,
    body_id: ElementPrimitiveId,
    bar_rail: Rc<RefCell<UiPrimitive>>,
    bar: Rc<RefCell<UiPrimitive>>
}

impl PartialEq for ScrollElement
{
    fn eq(&self, other: &Self) -> bool
    {
        self.body_id == other.body_id
    }
}

impl ScrollElement
{
    fn new_with<T: TopLevelAdder>(
        info: ScrollElementInfo,
        adder: &mut ElementAdder<T>
    ) -> Self
    {
        let body_id = adder.top_level(UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: info.pos,
            size: info.size,
            texture: None
        });

        let _background = adder.child(&body_id, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2::repeat(0.0),
            size: Point2::repeat(1.0).into(),
            texture: Some(info.background)
        });

        let rail_size = 1.0 - info.bar_size;

        let bar_rail_id = adder.child(&body_id, UiElementPrimitive{
            kind: UiElementType::Button,
            pos: Point2{x: 0.0, y: (1.0 - rail_size) * 0.5},
            size: Point2{x: 1.0, y: rail_size}.into(),
            texture: None
        });

        let bar_id = adder.child(&bar_rail_id, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2::repeat(0.0),
            size: Point2{x: 1.0, y: info.bar_size / rail_size}.into(),
            texture: Some(info.scrollbar)
        });

        let body = adder.ui.get_primitive(&body_id);
        let bar_rail = adder.ui.get_primitive(&bar_rail_id);
        let bar = adder.ui.get_primitive(&bar_id);

        Self{
            held: false,
            dragging: false,
            scroll: 0.0,
            body,
            body_id,
            bar_rail,
            bar
        }
    }

    pub fn scroll(&self) -> f32
    {
        self.scroll
    }

    fn set_scroll(&mut self, new_scroll: f32)
    {
        self.scroll = new_scroll;

        self.update_bar();
    }

    fn update_bar(&mut self)
    {
        self.bar.set(UiAnimatableId::PositionCenteredY, self.scroll);
    }

    fn mouse_move(&mut self, pos: Point2<f32>)
    {
        if self.dragging
        {
            let pos = self.bar_rail.borrow().element().local_position(pos);

            self.set_scroll(pos.y.clamp(0.0, 1.0));
        }
    }

    fn mouse_state(&mut self, down: bool, pos: Point2<f32>)
    {
        self.held = down;

        if down
        {
            let inside = self.bar_rail.borrow().element().intersects(pos);
            let edge = self.body.borrow().element().intersects(pos);

            if inside || edge
            {
                self.dragging = true;
            }

            self.mouse_move(pos);
        } else
        {
            self.dragging = false;
        }
    }
}

pub struct Children<'a>(&'a [Item]);

impl<'a> Index<usize> for Children<'a>
{
    type Output = ElementId;

    fn index(&self, index: usize) -> &Self::Output
    {
        &self.0[index].id
    }
}

#[derive(Debug, Clone)]
struct Item
{
    id: ElementId,
    frame: Rc<RefCell<UiPrimitive>>,
    value: Rc<RefCell<UiPrimitive>>
}

#[derive(Debug, Clone)]
pub struct ListElement
{
    items: Vec<Item>,
    background: Rc<RefCell<UiPrimitive>>,
    body_id: ElementPrimitiveId,
    scroll: Rc<RefCell<UiComplex>>,
    item_height: f32,
    last_scroll: f32,
    draw_range: Range<usize>
}

impl PartialEq for ListElement
{
    fn eq(&self, other: &Self) -> bool
    {
        self.body_id == other.body_id
    }
}

impl ListElement
{
    fn new_with<T: TopLevelAdder>(
        info: ListElementInfo,
        adder: &mut ElementAdder<T>
    ) -> Self
    {
        let body_id = ElementId::Primitive(adder.top_level(UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: info.pos,
            size: info.size,
            texture: None
        }));

        let scrollbar_width = 0.1;
        let bar_size = 0.1;

        let scroll = UiElementComplex::Scroll(ScrollElementInfo{
            pos: Point2{x: 1.0 - scrollbar_width, y: 0.0},
            size: Point2{x: scrollbar_width, y: 1.0}.into(),
            bar_size,
            background: info.scroll_background,
            scrollbar: info.scrollbar
        });

        let scroll = scroll.new_child(&mut adder.ui, &body_id);
        let scroll = adder.ui.get_complex(scroll).clone();

        // insanity
        let body_id = if let ElementId::Primitive(x) = body_id
        {
            x
        } else
        {
            unreachable!()
        };

        let background_id = adder.child(&body_id, UiElementPrimitive{
            kind: UiElementType::Panel,
            pos: Point2{x: 0.0, y: 0.0},
            size: Point2{x: 1.0 - scrollbar_width, y: 1.0}.into(),
            texture: Some(info.background)
        }.default_flags().no_draw());

        let items = info.items.into_iter().map(|element|
        {
            let container = adder.child(&background_id, UiElementPrimitive{
                kind: UiElementType::Panel,
                pos: Point2{x: 0.0, y: 0.0},
                size: Point2{x: 1.0, y: info.item_height}.into(),
                texture: None
            });

            let id = adder.child(&container, element);

            Item{
                value: adder.ui.get_primitive(&id).clone(),
                frame: adder.ui.get_primitive(&container).clone(),
                id: ElementId::Primitive(id)
            }
        }).collect();

        let background = adder.ui.get_primitive(&background_id).clone();

        let mut this = Self{
            items,
            background,
            body_id,
            item_height: info.item_height,
            last_scroll: 0.0,
            scroll,
            draw_range: 0..0
        };

        this.update_scroll(this.get_scroll());

        this
    }

    pub fn children(&self) -> Children
    {
        Children(&self.items)
    }

    fn get_scroll(&self) -> f32
    {
        let scroll = self.scroll.borrow();

        match &*scroll
        {
            UiComplex::Scroll(x) => x.scroll(),
            _ => unreachable!()
        }
    }

    fn fits(&self) -> f32
    {
        1.0 / self.item_height
    }

    fn start_index(&self) -> f32
    {
        let fits = self.fits();

        let height = 1.0 - self.last_scroll;

        let last_item = self.items.len().saturating_sub(fits as usize);
        height * last_item as f32
    }

    fn drawing_range(&self) -> Range<usize>
    {
        let start_index = self.start_index();

        let offset = start_index.fract();
        let start_index = start_index as usize;

        let len = (self.fits() + offset).ceil() as usize;
        let end_index = (start_index + len).min(self.items.len());

        start_index..end_index
    }

    fn draw_custom(&self, ui: &Ui)
    {
        let background = &self.background.borrow().element;
        ui.draw_element(background);

        let parent_size = ui.pixels_size_inner(&background).map(|x| x as usize);

        let mut assets = ui.ui.assets.borrow_mut();
        let clip_texture = ui.ui.temporary_texture;

        let image = Image::repeat(parent_size.x, parent_size.y, Color{r: 0, g: 0, b: 0, a: 0});
        assets.update_texture(clip_texture, &image);

        let size = parent_size.map(|x| x as f32);

        self.items[self.draw_range.clone()].iter().for_each(|item|
        {
            let value = &item.value.borrow().element;
            if let Some(texture_id) = value.inner.texture
            {
                let (clip_texture, texture) = assets.get_two_mut(clip_texture, texture_id);

                let frame = &item.frame.borrow().element.inner;

                let pos = (frame.pos * size).map(|x| x as i32);

                let frame_size = frame.size.to_size(1.0);
                let size = (frame_size * size).map(|x| x as u32 + 1);

                let rect = Rect::new(
                    pos.x,
                    parent_size.y as i32 - pos.y - size.y as i32,
                    size.x,
                    size.y
                );

                ui.draw_texture_to_texture(clip_texture, texture, rect);
            }
        });

        ui.draw_texture(assets.texture(clip_texture), background);
    }

    fn update_scroll(&mut self, scroll: f32)
    {
        self.last_scroll = scroll;
        self.draw_range = self.drawing_range();
        
        let offset = self.start_index().fract();

        self.items[self.draw_range.clone()].iter_mut().enumerate().for_each(|(index, item)|
        {
            let y = (index as f32 - offset) * self.item_height;

            item.frame.set(UiAnimatableId::PositionY, 1.0 - y - self.item_height);
        });
    }

    // i dont know if i need the &Ui anymore, im too tired of constantly removing it
    fn mouse_move(&mut self, _ui: &Ui, _pos: Point2<f32>)
    {
        let scroll = self.get_scroll();

        if self.last_scroll != scroll
        {
            self.update_scroll(scroll);
        }
    }

    fn mouse_state(&mut self, ui: &Ui, _down: bool, pos: Point2<f32>)
    {
        self.mouse_move(ui, pos)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UiComplex
{
    Scroll(ScrollElement),
    List(ListElement)
}

impl UiComplex
{
    fn new_with<T: TopLevelAdder>(
        value: UiElementComplex,
        mut adder: ElementAdder<T>
    ) -> Self
    {
        match value
        {
            UiElementComplex::Scroll(x) => Self::Scroll(ScrollElement::new_with(x, &mut adder)),
            UiElementComplex::List(x) => Self::List(ListElement::new_with(x, &mut adder))
        }
    }

    fn draw_custom(&self, ui: &Ui)
    {
        match self
        {
            Self::List(x) => x.draw_custom(ui),
            _ => ()
        }
    }

    fn mouse_move(&mut self, ui: &Ui, pos: Point2<f32>)
    {
        match self
        {
            Self::Scroll(x) => x.mouse_move(pos),
            Self::List(x) => x.mouse_move(ui, pos)
        }
    }

    fn mouse_state(&mut self, ui: &Ui, down: bool, pos: Point2<f32>)
    {
        match self
        {
            Self::Scroll(x) => x.mouse_state(down, pos),
            Self::List(x) => x.mouse_state(ui, down, pos)
        }
    }
}

// i could just store the children in a vec but this is much cooler
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementPrimitiveId
{
    id: usize,
    child: Option<Box<Self>>
}

impl ElementPrimitiveId
{
    pub fn new(id: usize) -> Self
    {
        Self{id, child: None}
    }

    pub fn push(&self, child_id: usize) -> Self
    {
        let mut element = self.clone();

        element.set_tail(child_id);

        element
    }

    pub fn set_tail(&mut self, child_id: usize)
    {
        if let Some(child_element) = self.child.as_mut()
        {
            child_element.set_tail(child_id)
        } else
        {
            self.child = Some(Box::new(Self::new(child_id)));
        }
    }
}

pub struct UiEvent
{
    pub element_id: ElementPrimitiveId
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub enum UiElementType
{
    Panel,
    Button
}

#[derive(Debug, Clone, Copy)]
pub struct KeepAspect
{
    aspect: f32,
    size: Point2<f32>
}

impl From<Point2<f32>> for KeepAspect
{
    fn from(size: Point2<f32>) -> Self
    {
        Self{aspect: 1.0, size}
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UiSize
{
    KeepAspect(KeepAspect),
    Normal(Point2<f32>)
}

impl From<KeepAspect> for UiSize
{
    fn from(value: KeepAspect) -> Self
    {
        Self::KeepAspect(value)
    }
}

impl From<Point2<f32>> for UiSize
{
    fn from(value: Point2<f32>) -> Self
    {
        Self::Normal(value)
    }
}

impl UiSize
{
    pub fn to_size(self, local_aspect: f32) -> Point2<f32>
    {
        match self
        {
            Self::Normal(x) => x,
            Self::KeepAspect(KeepAspect{aspect, size: p}) =>
            {
                let aspect = aspect * local_aspect;

                if aspect > 1.0
                {
                    Point2{x: p.x / aspect, ..p}
                } else
                {
                    Point2{y: p.y * aspect, ..p}
                }
            }
        }
    }

    fn set_aspect(&mut self, new_aspect: f32)
    {
        match self
        {
            Self::KeepAspect(KeepAspect{aspect, ..}) => *aspect = new_aspect,
            _ => ()
        }
    }

    pub fn set_x(&mut self, x: f32)
    {
        self.inner_mut().x = x;
    }

    pub fn set_y(&mut self, y: f32)
    {
        self.inner_mut().y = y;
    }

    #[allow(dead_code)]
    pub fn map(mut self, f: impl FnOnce(Point2<f32>) -> Point2<f32>) -> Self
    {
        let p = self.inner_mut();

        *p = f(*p);

        self
    }

    fn inner_mut(&mut self) -> &mut Point2<f32>
    {
        match self
        {
            Self::Normal(x) => x,
            Self::KeepAspect(KeepAspect{size, ..}) => size
        }
    }
}

#[derive(Debug, Clone)]
struct PrimitiveFlags
{
    no_draw: bool
}

impl Default for PrimitiveFlags
{
    fn default() -> Self
    {
        Self{no_draw: false}
    }
}

#[derive(Debug, Clone)]
pub struct UiElementPrimitiveWithFlags
{
    inner: UiElementPrimitive,
    flags: PrimitiveFlags
}

impl From<UiElementPrimitive> for UiElementPrimitiveWithFlags
{
    fn from(x: UiElementPrimitive) -> Self
    {
        x.default_flags()
    }
}

impl UiElementPrimitiveWithFlags
{
    pub fn no_draw(mut self) -> Self
    {
        self.flags.no_draw = true;

        self
    }
}

pub enum UiElement
{
    Complex(UiElementComplex),
    Primitive(UiElementPrimitiveWithFlags)
}

impl From<UiElementPrimitiveWithFlags> for UiElement
{
    fn from(x: UiElementPrimitiveWithFlags) -> Self
    {
        Self::Primitive(x)
    }
}

impl From<UiElementPrimitive> for UiElement
{
    fn from(x: UiElementPrimitive) -> Self
    {
        UiElementPrimitiveWithFlags::from(x).into()
    }
}

impl From<UiElementComplex> for UiElement
{
    fn from(x: UiElementComplex) -> Self
    {
        Self::Complex(x)
    }
}

impl UiElement
{
    fn new(self, ui: &mut Ui) -> ElementId
    {
        match self
        {
            Self::Complex(x) => ElementId::Complex(x.new(ui)),
            Self::Primitive(x) => ElementId::Primitive(x.new(ui))
        }
    }

    fn new_child(self, ui: &mut Ui, parent: &ElementId) -> ElementId
    {
        match self
        {
            Self::Complex(x) => ElementId::Complex(x.new_child(ui, parent)),
            Self::Primitive(x) => ElementId::Primitive(x.new_child(ui, parent))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScrollElementInfo
{
    pub pos: Point2<f32>,
    pub size: UiSize,
    pub bar_size: f32,
    pub background: TextureId,
    pub scrollbar: TextureId
}

#[derive(Debug, Clone)]
pub struct ListElementInfo
{
    pub items: Vec<UiElementPrimitive>,
    pub pos: Point2<f32>,
    pub size: UiSize,
    pub item_height: f32,
    pub background: TextureId,
    pub scroll_background: TextureId,
    pub scrollbar: TextureId
}

#[derive(Debug, Clone)]
pub enum UiElementComplex
{
    Scroll(ScrollElementInfo),
    List(ListElementInfo)
}

impl UiElementComplex
{
    fn new(self, ui: &mut Ui) -> ComplexId
    {
        let element = UiComplex::new_with(self, ElementAdder{ui, adder: TopLevelAdderNormal});

        Self::into_id(ui, element)
    }

    fn new_child(self, ui: &mut Ui, parent: &ElementId) -> ComplexId
    {
        let element = UiComplex::new_with(
            self,
            ElementAdder{ui, adder: TopLevelAdderChild{parent}}
        );

        Self::into_id(ui, element)
    }

    fn into_id(ui: &mut Ui, element: UiComplex) -> ComplexId
    {
        ui.push_complex(Rc::new(RefCell::new(element)))
    }
}

#[derive(Debug, Clone)]
pub struct UiElementPrimitive
{
    pub kind: UiElementType,
    pub pos: Point2<f32>,
    pub size: UiSize,
    pub texture: Option<TextureId>
}

impl UiElementPrimitive
{
    pub fn default_flags(self) -> UiElementPrimitiveWithFlags
    {
        UiElementPrimitiveWithFlags{
            inner: self,
            flags: Default::default()
        }
    }
}

impl UiElementPrimitiveWithFlags
{
    fn new(self, ui: &mut Ui) -> ElementPrimitiveId
    {
        let id = ui.ui.elements.len();

        let element = UiPrimitive::new_parent(self);
        UiPrimitive::full_update(element.clone(), ui.aspect());

        ui.ui.elements.push(element);

        ElementPrimitiveId::new(id)
    }

    fn new_child(self, ui: &Ui, parent: &ElementId) -> ElementPrimitiveId
    {
        self.new_child_inner(ui, parent.primitive_id(ui))
    }

    fn new_child_inner<'a>(
        self,
        ui: &Ui,
        parent: impl Into<PrimitiveIdWrapper<'a>>
    ) -> ElementPrimitiveId
    {
        let parent = parent.into();
        let parent = parent.as_ref();

        let id = UiPrimitive::push(&ui.get_primitive(parent), self, ui.aspect());

        parent.push(id)
    }
}

#[derive(Debug, Clone)]
pub struct UiElementGlobal
{
    inner: UiElementPrimitive,
    flags: PrimitiveFlags,
    global_size: Point2<f32>,
    global_pos: Point2<f32>
}

impl UiElementGlobal
{
    pub fn intersects(&self, pos: Point2<f32>) -> bool
    {
        (self.global_pos.x..=(self.global_pos.x + self.global_size.x)).contains(&pos.x)
            && (self.global_pos.y..=(self.global_pos.y + self.global_size.y)).contains(&pos.y)
    }

    pub fn local_position(&self, pos: Point2<f32>) -> Point2<f32>
    {
        (pos - self.global_pos) / self.global_size
    }

    pub fn inside_position(&self, pos: Point2<f32>) -> Option<Point2<f32>>
    {
        self.intersects(pos).then(|| self.local_position(pos))
    }

    fn set_aspect(&mut self, aspect: f32)
    {
        self.inner.size.set_aspect(aspect);
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum UiAnimatableId
{
    ScaleX,
    ScaleY,
    PositionX,
    PositionY,
    PositionCenteredX,
    PositionCenteredY
}

#[derive(Debug)]
pub struct UiPrimitive
{
    parent: Option<(usize, Weak<RefCell<Self>>)>,
    element: UiElementGlobal,
    children: Vec<Rc<RefCell<Self>>>
}

impl UiPrimitive
{
    #[allow(dead_code)]
    pub fn texture(&mut self) -> &mut Option<TextureId>
    {
        &mut self.element.inner.texture
    }

    pub fn element(&self) -> &UiElementGlobal
    {
        &self.element
    }

    fn new_parent(element: UiElementPrimitiveWithFlags) -> Rc<RefCell<Self>>
    {
        Self::new_inner(None, element)
    }

    fn new_child(
        parent: Weak<RefCell<Self>>,
        id: usize,
        element: UiElementPrimitiveWithFlags
    ) -> Rc<RefCell<Self>>
    {
        Self::new_inner(Some((id, parent)), element)
    }

    fn new_inner(
        parent: Option<(usize, Weak<RefCell<Self>>)>,
        element: UiElementPrimitiveWithFlags
    ) -> Rc<RefCell<Self>>
    {
        let zero = Point2::<f32>::zero();

        Rc::new(RefCell::new(Self{
            parent,
            element: UiElementGlobal{
                global_size: zero,
                global_pos: zero,
                inner: element.inner,
                flags: element.flags
            },
            children: Vec::new()
        }))
    }

    fn push(
        this: &Rc<RefCell<Self>>,
        element: UiElementPrimitiveWithFlags,
        aspect: f32
    ) -> usize
    {
        let parent = this.clone();

        let id;
        let child;

        {
            let mut this = this.borrow_mut();

            id = this.children.len();

            child = Self::new_child(Rc::downgrade(&parent), id, element);

            this.children.push(child.clone());
        }

        Self::full_update(child, aspect);

        id
    }

    fn update_child(&mut self, id: usize)
    {
        let mut child = self.children[id].borrow_mut();
        let this = &mut self.element;
        
        {
            let child = &mut child.element;

            let aspect = this.global_size.aspect();

            child.global_pos = this.global_pos + child.inner.pos * this.global_size;
            child.global_size = child.inner.size.to_size(aspect) * this.global_size;
        }

        child.update_children();
    }

    fn update_children(&mut self)
    {
        for i in 0..self.children.len()
        {
            self.update_child(i);
        }
    }

    fn full_update(this: Rc<RefCell<Self>>, aspect: f32)
    {
        this.borrow_mut().update_aspect(aspect);
        Self::update(this);
    }

    fn update_aspect(&mut self, aspect: f32)
    {
        self.element.set_aspect(aspect);

        for child in self.children.iter()
        {
            child.borrow_mut().update_aspect(aspect);
        }
    }

    fn update(this: Rc<RefCell<Self>>)
    {
        let pair = this.borrow().parent.clone();
        if let Some((id, parent)) = pair
        {
            parent.upgrade().unwrap().borrow_mut().update_child(id);
        } else
        {
            let mut this = this.borrow_mut();

            this.element.global_pos = this.element.inner.pos;
            this.element.global_size = this.element.inner.size.to_size(1.0);

            this.update_children();
        }
    }

    pub fn size(&self) -> Point2<f32>
    {
        let aspect = if let Some((_id, parent)) = self.parent.clone()
        {
            parent.upgrade().unwrap().borrow().element.global_size.aspect()
        } else
        {
            1.0
        };

        self.element.inner.size.to_size(aspect)
    }

    fn get(&self, id: &ElementPrimitiveId) -> Rc<RefCell<Self>>
    {
        let this = &self.children[id.id];

        if let Some(child_id) = id.child.as_ref()
        {
            this.borrow().get(child_id)
        } else
        {
            this.clone()
        }
    }

    fn try_for_each_element<T, Q, F>(
        &self,
        query: &Q,
        id: ElementPrimitiveId,
        f: &mut F
    ) -> ControlFlow<T>
    where
        Q: Fn(&Self) -> bool,
        F: FnMut(&ElementPrimitiveId, &UiElementGlobal) -> ControlFlow<T>
    {
        match f(&id, &self.element)
        {
            ControlFlow::Continue(_) => (),
            x => return x
        }

        self.children.iter().enumerate().try_for_each(|(index, child)|
        {
            let child = child.borrow();

            if query(&child)
            {
                let id = id.push(index);

                child.try_for_each_element(query, id, f)
            } else
            {
                ControlFlow::Continue(())
            }
        })
    }
}

impl Animatable<UiAnimatableId> for Rc<RefCell<UiPrimitive>>
{
    fn set(&mut self, id: UiAnimatableId, value: f32)
    {
        {
            let mut this = self.borrow_mut();
            let size = this.size();

            let element = &mut this.element.inner;
            match id
            {
                UiAnimatableId::ScaleX =>
                {
                    element.size.set_x(value);
                },
                UiAnimatableId::ScaleY =>
                {
                    element.size.set_y(value);
                },
                UiAnimatableId::PositionX =>
                {
                    element.pos.x = value;
                },
                UiAnimatableId::PositionY =>
                {
                    element.pos.y = value;
                },
                UiAnimatableId::PositionCenteredX =>
                {
                    element.pos.x = value - (size.x * 0.5);
                },
                UiAnimatableId::PositionCenteredY =>
                {
                    element.pos.y = value - (size.y * 0.5);
                }
            }
        }

        UiPrimitive::update(self.clone());
    }
}

struct UiGeneral
{
    window: Rc<RefCell<WindowWrapper>>,
    assets: Rc<RefCell<Assets>>,
    elements: Vec<Rc<RefCell<UiPrimitive>>>,
    temporary_texture: TextureId
}

pub struct Ui
{
    complex: Vec<Rc<RefCell<UiComplex>>>,
    // i thought i needed a separate reference but im stupid ahhhhhhh, wutever
    ui: UiGeneral
}

#[allow(dead_code)]
impl Ui
{
    pub fn new(window: Rc<RefCell<WindowWrapper>>, assets: Rc<RefCell<Assets>>) -> Self
    {
        let empty_image = Image::repeat(1, 1, Color{r: 0, g: 0, b: 0, a: 0});
        let temporary_texture = assets.borrow_mut().add_texture_access(
            TextureAccess::Target,
            &empty_image
        );

        let ui = UiGeneral{
            window,
            assets,
            elements: Vec::new(),
            temporary_texture
        };

        Self{complex: Vec::new(), ui}
    }

    pub fn push(&mut self, element: impl Into<UiElement>) -> ElementId
    {
        element.into().new(self)
    }

    pub fn push_child(
        &mut self,
        parent_id: &ElementId,
        element: impl Into<UiElement>
    ) -> ElementId
    {
        element.into().new_child(self, parent_id)
    }

    pub fn get(&self, id: &ElementId) -> Rc<RefCell<UiPrimitive>>
    {
        let id = id.primitive_id(self);

        self.get_primitive(id.as_ref())
    }

    pub fn get_primitive<'a>(
        &self,
        id: impl TryInto<&'a ElementPrimitiveId>
    ) -> Rc<RefCell<UiPrimitive>>
    {
        let id = id.try_into().unwrap_or_else(|_| panic!("id isnt primitive"));
        let this = &self.ui.elements[id.id];

        if let Some(child_id) = id.child.as_ref()
        {
            this.borrow().get(child_id)
        } else
        {
            this.clone()
        }
    }

    pub fn get_complex(&self, id: impl TryInto<ComplexId>) -> &Rc<RefCell<UiComplex>>
    {
        let id = id.try_into().unwrap_or_else(|_| panic!("id isnt primitive"));
        &self.complex[id.0]
    }

    fn complex_to_primitive(&self, id: ComplexId) -> Ref<ElementPrimitiveId>
    {
        let complex = self.get_complex(id);

        Ref::map(complex.borrow(), |complex|
        {
            match complex
            {
                UiComplex::Scroll(x) => &x.body_id,
                UiComplex::List(x) => &x.body_id
            }
        })
    }

    fn push_complex(&mut self, element: Rc<RefCell<UiComplex>>) -> ComplexId
    {
        let id = self.complex.len();

        self.complex.push(element);

        ComplexId(id)
    }

    pub fn pixels_size(&self, id: &ElementId) -> Point2<u32>
    {
        let ele = self.get(id);
        let ele = ele.borrow();

        self.pixels_size_inner(&ele.element)
    }

    fn pixels_size_inner(&self, element: &UiElementGlobal) -> Point2<u32>
    {
        let window_size = self.window_size().map(|x| x as f32);

        (element.global_size * window_size)
            .map(|x| x.round() as u32)
    }

    fn aspect(&self) -> f32
    {
        self.window_size().map(|x| x as f32).aspect()
    }

    fn window_size(&self) -> Point2<u32>
    {
        self.ui.window.borrow().window_size()
    }

    pub fn resized(&mut self)
    {
        for element in self.ui.elements.iter()
        {
            UiPrimitive::full_update(element.clone(), self.aspect());
        }
    }

    fn temporary_texture(&self, size: Point2<usize>) -> RefMut<Texture<'static>>
    {
        let image = Image::repeat(size.x, size.y, Color{r: 0, g: 0, b: 0, a: 0});

        RefMut::map(self.ui.assets.borrow_mut(), |assets|
        {
            assets.update_texture(self.ui.temporary_texture, &image)
        })
    }

    fn draw_element(&self, element: &UiElementGlobal)
    {
        if let Some(texture_id) = element.inner.texture
        {
            let assets = self.ui.assets.borrow();

            self.draw_texture(assets.texture(texture_id), element);
        }
    }

    fn draw_texture(&self, texture: &Texture, element: &UiElementGlobal)
    {
        let rect = self.element_rect(element);
        
        self.ui.window.borrow_mut().canvas
            .copy(texture, None, rect)
            .unwrap();
    }

    fn draw_to_texture(&self, texture: &mut Texture, element: &UiElementGlobal)
    {
        if let Some(texture_id) = element.inner.texture
        {
            let assets = self.ui.assets.borrow();

            let rect = self.element_rect(element);
            self.draw_texture_to_texture(texture, assets.texture(texture_id), rect)
        }
    }

    fn draw_texture_to_texture(
        &self,
        target_texture: &mut Texture,
        draw_texture: &Texture,
        rect: Rect
    )
    {
        self.ui.window.borrow_mut().canvas
            .with_texture_canvas(target_texture, |canvas|
            {
                canvas.copy(draw_texture, None, rect).unwrap()
            })
            .unwrap();
    }

    fn draw_with(&self, f: impl FnOnce(&Texture, Rect), element: &UiElementGlobal)
    {
        if let Some(texture_id) = element.inner.texture
        {
            let assets = self.ui.assets.borrow();

            let rect = self.element_rect(element);
            f(assets.texture(texture_id), rect)
        }
    }

    fn element_rect(&self, element: &UiElementGlobal) -> Rect
    {
        let window_size = self.window_size().map(|x| x as f32);

        let scaled_pos = {
            let mut pos = element.global_pos;

            pos.y = 1.0 - pos.y - element.global_size.y;

            pos * window_size
        }.map(|x| x.round() as i32);

        let x = scaled_pos.x;
        let y = scaled_pos.y;

        let Point2{x: width, y: height} = self.pixels_size_inner(element);

        Rect::new(x, y, width, height)
    }

    pub fn draw(&self)
    {
        self.for_each_element(&|element|
        {
            !element.element.flags.no_draw
        }, |_id, element|
        {
            self.draw_element(element);
        });

        self.complex.iter().for_each(|complex|
        {
            complex.borrow().draw_custom(self);
        });
    }

    pub fn mouse_move(&self, pos: Point2<f32>)
    {
        self.complex.iter().for_each(|complex|
        {
            complex.borrow_mut().mouse_move(self, pos);
        });
    }

    pub fn mouse_down(&self, pos: Point2<f32>) -> Option<UiEvent>
    {
        self.mouse_state(true, pos)
    }

    pub fn mouse_up(&self, pos: Point2<f32>) -> Option<UiEvent>
    {
        self.mouse_state(false, pos)
    }

    pub fn mouse_state(&self, down: bool, pos: Point2<f32>) -> Option<UiEvent>
    {
        self.complex.iter().for_each(|complex|
        {
            complex.borrow_mut().mouse_state(self, down, pos);
        });

        let flow = self.try_for_each_element(&|_| true, |id, element|
        {
            match element.inner.kind
            {
                UiElementType::Button =>
                {
                    if down && element.intersects(pos)
                    {
                        return ControlFlow::Break(UiEvent{element_id: id.clone()});
                    }
                },
                UiElementType::Panel => ()
            }

            ControlFlow::Continue(())
        });

        match flow
        {
            ControlFlow::Break(x) => Some(x),
            ControlFlow::Continue(_) => None
        }
    }

    fn try_for_each_element<T, Q, F>(&self, query: &Q, mut f: F) -> ControlFlow<T>
    where
        Q: Fn(&UiPrimitive) -> bool,
        F: FnMut(&ElementPrimitiveId, &UiElementGlobal) -> ControlFlow<T>
    {
        self.ui.elements.iter().enumerate().try_for_each(|(index, element)|
        {
            let element = element.borrow();

            if query(&element)
            {
                let id = ElementPrimitiveId::new(index);

                element.try_for_each_element(query, id, &mut f)
            } else
            {
                ControlFlow::Continue(())
            }
        })
    }

    fn for_each_element<Q, F>(&self, query: &Q, mut f: F)
    where
        Q: Fn(&UiPrimitive) -> bool,
        F: FnMut(&ElementPrimitiveId, &UiElementGlobal)
    {
        self.try_for_each_element(query, |id, element|
        {
            f(id, element);

            ControlFlow::<()>::Continue(())
        });
    }
}
