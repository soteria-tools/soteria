module L = struct
  let sink fmt = Format.ifprintf Format.std_formatter fmt
  let debug f = f sink
  let info f = f sink
  let warn f = f sink
  let error f = f sink
  let trace f = f sink
  let smt f = f sink
end
