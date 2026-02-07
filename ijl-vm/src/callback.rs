use gc_arena::Gc;
use std::marker::PhantomData;

pub enum CallbackReturn<'gc> {
    Return,
    _Phantom(PhantomData<&'gc ()>),
}

pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

pub struct CallbackInner<'gc> {
    call: unsafe fn(*const CallbackInner<'gc>) -> CallbackReturn<'gc>,
}
