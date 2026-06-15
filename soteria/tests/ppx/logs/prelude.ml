module L = struct
  module Level = struct
    type t = Smt | Trace | Debug | Info | Warn | Error
  end

  let should_log (_ : Level.t) = false

  let force_log ~level:(_ : Level.t) fmt =
    Format.ifprintf Format.std_formatter fmt
end
