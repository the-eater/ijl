use crate::t::MetricVec;
use crate::{Context, Value};
use gc_arena::{Collect, Gc, Mutation};
use std::marker::PhantomData;

pub enum CallbackReturn<'gc> {
    Return,
    _Phantom(PhantomData<&'gc ()>),
}

pub trait CallbackFn<'gc>: Collect {
    fn call(&self, ctx: Context<'gc>, stack: Stack<'gc, '_>) -> CallbackReturn<'gc>;
}

pub struct Callback<'gc>(Gc<'gc, CallbackInner<'gc>>);

pub struct CallbackInner<'gc> {
    call: unsafe fn(*const CallbackInner<'gc>, Context<'gc>, Stack<'gc, '_>) -> CallbackReturn<'gc>,
}

impl<'gc> Callback<'gc> {
    pub fn new<C: CallbackFn<'gc> + 'gc>(mc: &Mutation<'gc>, callback: C) -> Self {
        #[repr(C)]
        struct HeaderCallback<'gc, C> {
            header: CallbackInner<'gc>,
            callback: C,
        }

        // SAFETY: We can't auto-implement `Collect` due to the function pointer lifetimes, but
        // function pointers can't hold any data.
        unsafe impl<'gc, C: Collect> Collect for HeaderCallback<'gc, C> {
            fn needs_trace() -> bool
            where
                Self: Sized,
            {
                C::needs_trace()
            }

            fn trace(&self, cc: &gc_arena::Collection) {
                self.callback.trace(cc)
            }
        }

        let hc = Gc::new(
            mc,
            HeaderCallback {
                header: CallbackInner {
                    call: |ptr, ctx, stack| unsafe {
                        let hc = ptr as *const HeaderCallback<C>;
                        ((*hc).callback).call(ctx, stack)
                    },
                },
                callback,
            },
        );

        Self(unsafe { Gc::cast::<CallbackInner>(hc) })
    }

    pub fn from_fn<F>(mc: &Mutation<'gc>, call: F) -> Callback<'gc>
    where
        F: 'static
            + Fn(
                Context<'gc>,
                Stack<'gc, '_>,
            ) -> CallbackReturn<'gc>,
    {
        Self::from_fn_with(mc, (), move |_, ctx, exec, stack| call(ctx, stack))
    }

    /// Create a callback from a Rust function together with a GC object.
    pub fn from_fn_with<R, F>(mc: &Mutation<'gc>, root: R, call: F) -> Callback<'gc>
    where
        R: 'gc + Collect,
        F: 'static
            + Fn(
                &R,
                Context<'gc>,
                Stack<'gc, '_>,
            ) -> CallbackReturn<'gc>,
    {
        #[derive(Collect)]
        #[collect(no_drop)]
        struct RootCallback<R, F> {
            root: R,
            #[collect(require_static)]
            call: F,
        }

        impl<'gc, R, F> CallbackFn<'gc> for RootCallback<R, F>
        where
            R: 'gc + Collect,
            F: 'static
                + Fn(
                    &R,
                    Context<'gc>,
                    Stack<'gc, '_>,
                ) -> CallbackReturn<'gc>,
        {
            fn call(
                &self,
                ctx: Context<'gc>,
                stack: Stack<'gc, '_>,
            ) -> CallbackReturn<'gc> {
                (self.call)(&self.root, ctx, stack)
            }
        }

        Callback::new(mc, RootCallback { root, call })
    }
}

pub struct Stack<'gc, 'a> {
    stack: &'a mut MetricVec<'gc, Value<'gc>>,
    offset: usize,
}
