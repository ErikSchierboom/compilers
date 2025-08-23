use leptos::prelude::*;
use compiler::interpreter::*;

/// A parameterized incrementing button
#[component]
pub fn Button(#[prop(default = 1)] increment: i32) -> impl IntoView {
    let (count, set_count) = signal(0);
    let x = interpret("1 2 + 7 *");
    let y = x.unwrap();
    let interpret_result = y.first().unwrap();
    let v = interpret_result.value.values.get(0).unwrap().to_string();

    view! {
        <button on:click=move |_| {
            set_count.set(count.get() + increment)
        }>

            "Click me: " {v}
        </button>
    }
}
