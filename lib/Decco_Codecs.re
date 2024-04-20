let falseableEncode = (encoder, opt) =>
  switch (opt) {
  | None => Js.Json.boolean(false)
  | Some(v) => encoder(v)
  };
let falseableDecode = (decoder, json) =>
  switch (Js.Json.decodeBoolean(json)) {
  | Some(false) => Ok(None)
  | _ => decoder(json) |> Stdlib.Result.map(v => Some(v))
  };
let falseable = (falseableEncode, falseableDecode);

let magicDecode = j => Ok(Obj.magic(j));
let magic = (Obj.magic, magicDecode);
