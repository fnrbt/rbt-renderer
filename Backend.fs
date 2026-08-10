namespace Renderer

type Backend =
    { Init: int -> int -> unit
      InitInto: Framebuffer -> unit
      Submit: Cmd list -> unit
      Finish: unit -> Framebuffer }
