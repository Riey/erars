struct Globals {
    screen: vec2<f32>,
    _pad: vec2<f32>,
};
@group(0) @binding(0) var<uniform> globals: Globals;
@group(0) @binding(1) var atlas_tex: texture_2d<f32>;
@group(0) @binding(2) var atlas_smp: sampler;

struct Instance {
    @location(0) rect: vec4<f32>,   // x, y, w, h in pixels
    @location(1) uv: vec4<f32>,     // u, v, uw, vh
    @location(2) color: vec4<f32>,  // rgba 0..1
    @location(3) mode: u32,         // 0 solid, 1 alpha-mask, 2 rgba
};

struct VsOut {
    @builtin(position) pos: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
    @location(2) @interpolate(flat) mode: u32,
};

@vertex
fn vs_main(@builtin(vertex_index) vid: u32, inst: Instance) -> VsOut {
    // Two triangles for a unit quad.
    var corners = array<vec2<f32>, 6>(
        vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 0.0), vec2<f32>(0.0, 1.0),
        vec2<f32>(0.0, 1.0), vec2<f32>(1.0, 0.0), vec2<f32>(1.0, 1.0),
    );
    let c = corners[vid];
    let px = inst.rect.xy + c * inst.rect.zw;
    // pixel -> NDC (y down to y up)
    let ndc = vec2<f32>(
        px.x / globals.screen.x * 2.0 - 1.0,
        1.0 - px.y / globals.screen.y * 2.0,
    );
    var out: VsOut;
    out.pos = vec4<f32>(ndc, 0.0, 1.0);
    out.uv = inst.uv.xy + c * inst.uv.zw;
    out.color = inst.color;
    out.mode = inst.mode;
    return out;
}

@fragment
fn fs_main(in: VsOut) -> @location(0) vec4<f32> {
    if (in.mode == 0u) {
        return in.color;
    } else if (in.mode == 1u) {
        let a = textureSample(atlas_tex, atlas_smp, in.uv).a;
        return vec4<f32>(in.color.rgb, in.color.a * a);
    } else {
        return textureSample(atlas_tex, atlas_smp, in.uv);
    }
}
