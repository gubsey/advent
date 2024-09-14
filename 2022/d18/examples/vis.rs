use std::{io::stdin, thread::sleep, time::Duration};

use macroquad::prelude::*;
use miniquad::window::set_window_size;

#[macroquad::main("hi")]
async fn main() {
    let voxels: Vec<_> = stdin()
        .lines()
        .map_while(Result::ok)
        .map(|s| s.split(",").map(|x| x.parse().unwrap()).collect::<Vec<_>>())
        .map(|v| vec3(v[0], v[1], v[2]))
        .collect();

    let mut t = 0.;
    let r = 35.;
    let center = 11.;

    loop {
        clear_background(LIGHTGRAY);

        t += get_frame_time() * 0.7;
        t %= 360.;

        let x = r * t.cos();
        let z = r * t.sin();

        set_camera(&Camera3D {
            position: vec3(x + center, 30. + center, z + center),
            up: vec3(0., 1., 0.),
            target: vec3(10., 10., 10.),
            ..Default::default()
        });

        for v in &voxels {
            draw_cube(*v, vec3(1., 1., 1.), None, BROWN);
            draw_cube_wires(*v, vec3(1., 1., 1.), BLACK);
        }

        set_default_camera();
        draw_text(t.to_string().as_str(), 5., 20., 30., BLACK);

        let ax = 40.;
        let y = screen_height() - ax;
        draw_line(ax, y, ax + x, y + z, 3., BLACK);

        next_frame().await;
    }
}
