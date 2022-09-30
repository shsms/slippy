use displays::Displays;
use std::{cell::RefCell, rc::Rc};
use swayipc_async::Fallible;
use tulisp::{tulisp_add_func, tulisp_fn, tulisp_fn_no_eval, Error, TulispContext, TulispValue};

mod displays;
mod window_transitions;
use window_transitions::WindowTransition;

#[derive(Default, Clone)]
struct State {
    window_transitions: Option<WindowTransition>,
    displays: Option<Displays>,
}

#[derive(Default, Clone)]
struct StateWrapper {
    state: Rc<RefCell<State>>,
}

impl StateWrapper {
    fn new(ctx: &mut TulispContext) -> Self {
        let wrapper = Self::default();
        let next = wrapper.clone();
        tulisp_add_func!(ctx, next.transitions, "transitions");
        let next = wrapper.clone();
        tulisp_add_func!(ctx, next.displays, "displays");
        wrapper
    }

    #[tulisp_fn]
    fn transitions(
        &self,
        duration_ms: i64,
        active_opacity: f64,
        inactive_opacity: f64,
        resolution_ms: Option<i64>,
    ) {
        let mut state = self.state.as_ref().borrow_mut();
        state.window_transitions = Some(WindowTransition::new(
            duration_ms,
            active_opacity,
            inactive_opacity,
            resolution_ms,
        ));
    }

    #[tulisp_fn]
    fn displays(&self, ctx: &mut TulispContext, rest: TulispValue) -> Result<TulispValue, Error> {
        let mut state = self.state.as_ref().borrow_mut();
        state.displays = Some(Displays::try_new(ctx, rest)?);
        Ok(TulispValue::nil())
    }
}

fn init_tulisp() -> State {
    let mut ctx = TulispContext::new();

    let state = StateWrapper::new(&mut ctx);

    ctx.eval_file("swaycfg.lisp").unwrap();

    let ret = state.state.as_ref().borrow().clone();
    ret
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let wt = init_tulisp();
    if let Some(disp) = wt.displays {
        disp.run().await;
    }
    if let Some(wt) = wt.window_transitions {
        wt.run().await;
    }
    Ok(())
}
