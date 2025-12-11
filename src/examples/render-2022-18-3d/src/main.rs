use std::io::stdin;

use macroquad::prelude::*;

#[macroquad::main("hi")]
async fn main() {
    let voxels: Vec<_> = stdin()
        .lines()
        .map_while(Result::ok)
        .map(|s| s.split(",").map(|x| x.parse().unwrap()).collect::<Vec<_>>())
        .map(|v| vec3(v[0], v[1], v[2]))
        .collect();

    let mut t = 0.;
    let r = 27.;
    let center = 11.;

    loop {
        clear_background(DARKBROWN);

        t += get_frame_time() * 0.7;
        t %= 360.;

        let x = r * t.cos() * 1.2;
        let y = r * t.sin() * 1.1;
        let z = r * t.sin() * 1.3;

        set_camera(&Camera3D {
            position: vec3(x + center, y + center, z + center),
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
