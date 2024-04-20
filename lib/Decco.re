type decodeError = {
  path: string,
  message: string,
  value: Js.Json.t,
};

type result('a) = Result.t('a, decodeError);
type decoder('a) = Js.Json.t => result('a);
type encoder('a) = 'a => Js.Json.t;
type codec('a) = (encoder('a), decoder('a));

let error = (~path=?, message, value) => {
  let path =
    switch (path) {
    | None => ""
    | Some(s) => s
    };
  Result.Error({path, message, value});
};

let stringToJson = s => Js.Json.string(s);
let stringFromJson = j =>
  switch (Js.Json.decodeString(j)) {
  | Some(s) => Ok(s)
  | None => Error({path: "", message: "Not a string", value: j})
  };

let intToJson = i => i |> float_of_int |> Js.Json.number;
let intFromJson = j =>
  switch (Js.Json.decodeNumber(j)) {
  | Some(f) =>
    float_of_int(Js.Math.floor_int(f)) == f
      ? Ok(Js.Math.floor_int(f))
      : Error({path: "", message: "Not an integer", value: j})

  | _ => Error({path: "", message: "Not a number", value: j})
  };

let int64ToJson = i => i |> Int64.float_of_bits |> Js.Json.number;

let int64FromJson = j =>
  switch (Js.Json.decodeNumber(j)) {
  | Some(n) => Ok(Int64.bits_of_float(n))
  | None => error("Not a number", j)
  };

let int64ToJsonUnsafe = i => i |> Int64.to_float |> Js.Json.number;

let int64FromJsonUnsafe = j =>
  switch (Js.Json.decodeNumber(j)) {
  | Some(n) => Ok(Int64.of_float(n))
  | None => error("Not a number", j)
  };

let floatToJson = v => v |> Js.Json.number;
let floatFromJson = j =>
  switch (Js.Json.decodeNumber(j)) {
  | Some(f) => Ok(f)
  | None => Error({path: "", message: "Not a number", value: j})
  };

let boolToJson = v => v |> Js.Json.boolean;
let boolFromJson = j =>
  switch (Js.Json.decodeBoolean(j)) {
  | Some(b) => Ok(b)
  | None => Error({path: "", message: "Not a boolean", value: j})
  };

let unitToJson = () => Js.Json.number(0.0);
let unitFromJson = _ => Ok();

let arrayToJson = (encoder, arr) =>
  arr |> Js.Array.map(~f=encoder) |> Js.Json.array;

let arrayFromJson = (decoder, json) =>
  switch (Js.Json.decodeArray(json)) {
  | Some(arr) =>
    Js.Array.reducei(
      ~init=Ok([||]),
      ~f=
        (acc, jsonI, i) => {
          switch (acc, decoder(jsonI)) {
          | (Error(_), _) => acc

          | (_, Error({path, _} as error)) =>
            Error({...error, path: "[" ++ string_of_int(i) ++ "]" ++ path})

          | (Ok(prev), Ok(newVal)) =>
            Ok(prev |> Js.Array.concat(~other=[|newVal|]))
          }
        },
      arr,
    )

  | None => Error({path: "", message: "Not an array", value: json})
  };

let listToJson = (encoder, list) =>
  list |> Array.of_list |> arrayToJson(encoder);

let foo = Belt.Option.map

let listFromJson = (decoder, json) =>
  json |> arrayFromJson(decoder) |> Result.map(Array.to_list);

let optionToJson = (encoder, opt) =>
  switch (opt) {
  | Some(x) => encoder(x)
  | None => Js.Json.null
  };

let optionFromJson = (decoder, json) =>
  switch (Js.Nullable.return(json) |> Js.Nullable.toOption) {
  | None => Ok(None)
  | Some(json) => decoder(json) |> Result.map(v => Some(v))
  };

let resultToJson = (okEncoder, errorEncoder, result) =>
  (
    switch (result) {
    | Ok(v) => [|Js.Json.string("Ok"), okEncoder(v)|]
    | Error(e) => [|Js.Json.string("Error"), errorEncoder(e)|]
    }
  )
  |> Js.Json.array;

let resultFromJson = (okDecoder, errorDecoder, json) =>
  switch (Js.Json.decodeArray(json)) {
  | Some([|variantConstructorId, payload|]) =>
    switch (Js.Json.decodeString(variantConstructorId)) {
    | Some("Ok") => okDecoder(payload) |> Result.map(v => Ok(v))

    | Some("Error") =>
      switch (errorDecoder(payload)) {
      | Ok(v) => Ok(Error(v))
      | Error(e) => Error(e)
      }

    | Some(_) =>
      error("Expected either \"Ok\" or \"Error\"", variantConstructorId)
    | None => error("Not a string", variantConstructorId)
    }
  | Some(_) => error("Expected exactly 2 values in array", json)
  | None => error("Not an array", json)
  };

let dictToJson = (encoder, dict) =>
  dict |> Js.Dict.map(~f=(. a) => encoder(a), _) |> Js.Json.object_;

let dictFromJson = (decoder, json) =>
  switch (Js.Json.decodeObject(json)) {
  | Some(dict) =>
    dict->Js.Dict.entries
    |> Js.Array.reduce(~init=Ok(Js.Dict.empty()), ~f=(acc, (key, value)) =>
         switch (acc, decoder(value)) {
         | (Error(_), _) => acc

         | (_, Error({path, _} as error)) =>
           Error({...error, path: "." ++ key ++ path})

         | (Ok(prev), Ok(newVal)) =>
           let () = prev->Js.Dict.set(key, newVal);
           Ok(prev);
         }
       )
  | None => Error({path: "", message: "Not a dict", value: json})
  };

module Codecs = {
  include Decco_Codecs;
  let string = (stringToJson, stringFromJson);
  let int = (intToJson, intFromJson);
  let int64Unsafe = (int64ToJsonUnsafe, int64FromJsonUnsafe);
  let float = (floatToJson, floatFromJson);
  let bool = (boolToJson, boolFromJson);
  let array = (arrayToJson, arrayFromJson);
  let list = (listToJson, listFromJson);
  let option = (optionToJson, optionFromJson);
  let unit = (unitToJson, unitFromJson);
};
