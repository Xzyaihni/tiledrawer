use std::{
    rc::{Weak, Rc},
    cell::RefCell,
    ops::ControlFlow
};

use sdl2::rect::Rect;

use crate::{Point2, WindowWrapper, Assets, TextureId, animator::Animatable};


// i could just store the children in a vec but this is much cooler
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementId
{
    id: usize,
    child: Option<Box<ElementId>>
}

impl ElementId
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
    pub element_id: ElementId
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
                Point2{y: p.y * aspect * local_aspect, ..p}
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
pub struct UiElement
{
    pub kind: UiElementType,
    pub pos: Point2<f32>,
    pub size: UiSize,
    pub texture: Option<TextureId>
}

#[derive(Debug, Clone)]
pub struct UiElementGlobal
{
    inner: UiElement,
    global_size: Point2<f32>,
    global_pos: Point2<f32>,
}

impl UiElementGlobal
{
    pub fn intersects(&self, pos: Point2<f32>) -> bool
    {
        (self.global_pos.x..=(self.global_pos.x + self.global_size.x)).contains(&pos.x)
            && (self.global_pos.y..=(self.global_pos.y + self.global_size.y)).contains(&pos.y)
    }

    pub fn inside_position(&self, pos: Point2<f32>) -> Option<Point2<f32>>
    {
        self.intersects(pos).then(||
        {
            (pos - self.global_pos) / self.global_size
        })
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

pub struct UiElementInner
{
    parent: Option<(usize, Weak<RefCell<Self>>)>,
    element: UiElementGlobal,
    children: Vec<Rc<RefCell<Self>>>
}

impl UiElementInner
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

    fn new_parent(element: UiElement) -> Rc<RefCell<Self>>
    {
        Self::new_inner(None, element)
    }

    fn new_child(parent: Weak<RefCell<Self>>, id: usize, element: UiElement) -> Rc<RefCell<Self>>
    {
        Self::new_inner(Some((id, parent)), element)
    }

    fn new_inner(
        parent: Option<(usize, Weak<RefCell<Self>>)>,
        element: UiElement
    ) -> Rc<RefCell<Self>>
    {
        let zero = Point2::<f32>::zero();

        Rc::new(RefCell::new(Self{
            parent,
            element: UiElementGlobal{
                global_size: zero,
                global_pos: zero,
                inner: element
            },
            children: Vec::new()
        }))
    }

    fn push(this: &Rc<RefCell<Self>>, element: UiElement, aspect: f32) -> usize
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

    fn get(&self, id: &ElementId) -> Rc<RefCell<Self>>
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

    fn try_for_each_element<T, F>(&self, id: ElementId, f: &mut F) -> ControlFlow<T>
    where
        F: FnMut(&ElementId, &UiElementGlobal) -> ControlFlow<T>
    {
        match f(&id, &self.element)
        {
            ControlFlow::Continue(_) => (),
            x => return x
        }

        self.children.iter().enumerate().try_for_each(|(index, child)|
        {
            let id = id.push(index);

            child.borrow().try_for_each_element(id, f)
        })
    }
}

impl Animatable<UiAnimatableId> for Rc<RefCell<UiElementInner>>
{
    fn set(&mut self, id: &UiAnimatableId, value: f32)
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

        UiElementInner::update(self.clone());
    }
}

pub struct Ui
{
    window: Rc<RefCell<WindowWrapper>>,
    assets: Rc<RefCell<Assets>>,
    elements: Vec<Rc<RefCell<UiElementInner>>>
}

#[allow(dead_code)]
impl Ui
{
    pub fn new(window: Rc<RefCell<WindowWrapper>>, assets: Rc<RefCell<Assets>>) -> Self
    {
        Self{window, assets, elements: Vec::new()}
    }

    pub fn push(&mut self, element: UiElement) -> ElementId
    {
        let id = self.elements.len();

        let element = UiElementInner::new_parent(element);
        UiElementInner::full_update(element.clone(), self.aspect());

        self.elements.push(element);

        ElementId::new(id)
    }

    pub fn push_child(&mut self, parent_id: &ElementId, element: UiElement) -> ElementId
    {
        let id = UiElementInner::push(&self.get(parent_id), element, self.aspect());

        parent_id.push(id)
    }

    pub fn get(&self, id: &ElementId) -> Rc<RefCell<UiElementInner>>
    {
        let this = &self.elements[id.id];

        if let Some(child_id) = id.child.as_ref()
        {
            this.borrow().get(child_id)
        } else
        {
            this.clone()
        }
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
        self.window.borrow().window_size()
    }

    pub fn resized(&mut self)
    {
        for element in self.elements.iter()
        {
            UiElementInner::full_update(element.clone(), self.aspect());
        }
    }

    pub fn draw(&self)
    {
        let window_size = self.window_size().map(|x| x as f32);

        let assets = self.assets.borrow();

        self.for_each_element(|_id, element|
        {
            if let Some(texture_id) = element.inner.texture
            {
                let texture = assets.texture(texture_id);

                let scaled_pos = {
                    let mut pos = element.global_pos;

                    pos.y = 1.0 - pos.y - element.global_size.y;

                    pos * window_size
                }.map(|x| x.round() as i32);

                let x = scaled_pos.x;
                let y = scaled_pos.y;

                let Point2{x: width, y: height} = self.pixels_size_inner(element);

                self.window.borrow_mut().canvas
                    .copy(texture, None, Rect::new(x, y, width, height))
                    .unwrap();
            }
        });
    }

    pub fn click(&self, pos: Point2<f32>) -> Option<UiEvent>
    {
        let flow = self.try_for_each_element(|id, element|
        {
            match element.inner.kind
            {
                UiElementType::Button =>
                {
                    if element.intersects(pos)
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

    fn try_for_each_element<T, F>(&self, mut f: F) -> ControlFlow<T>
    where
        F: FnMut(&ElementId, &UiElementGlobal) -> ControlFlow<T>
    {
        self.elements.iter().enumerate().try_for_each(|(index, element)|
        {
            let id = ElementId::new(index);

            element.borrow().try_for_each_element(id, &mut f)
        })
    }

    fn for_each_element<F>(&self, mut f: F)
    where
        F: FnMut(&ElementId, &UiElementGlobal)
    {
        self.try_for_each_element(|id, element|
        {
            f(id, element);

            ControlFlow::<()>::Continue(())
        });
    }
}
