module Xhr = {
  type xhr;
  type xhrResponse;
  [@bs.new] external xmlHttpRequest: unit => xhr = "XMLHttpRequest";
  [@bs.get] external response: xhr => xhrResponse = "";
  [@bs.set] external responseType: (xhr, string) => unit = "";
  [@bs.set] external onLoad: (xhr, unit => unit) => unit = "onload";
  [@bs.send.pipe: xhr] external send: unit => unit = "";
  [@bs.send.pipe: xhr]
  external openUri: ([@bs.string] [ | `GET | `POST], string, bool) => unit =
    "open";
  /*
    Returns an array of bytes from the arrayBuffer
   */
  let toArray: xhrResponse => array(int) = [%bs.raw
    {|function(response) { return new Uint8Array(response); }|}
  ];
  let openUri = (method_, uri: string, xhr: xhr) =>
    Js.Promise.make((~resolve, ~reject as _) => {
      onLoad(xhr, () => resolve(. response(xhr) |> toArray));
      responseType(xhr, "arraybuffer");
      xhr |> openUri(method_, uri, true);
      xhr |> send();
    });
};

module Canvas = {
  type ctx;
  type imageData;
  let getContextFromId: string => ctx = [%bs.raw
    {|
      function(id) {
        const canvas = document.getElementById(id)
        return canvas.getContext("2d");
      }
  |}
  ];
  [@bs.send.pipe: ctx] external createImageData: (int, int) => imageData = "";
  [@bs.send.pipe: ctx]
  external putImageData: (imageData, int, int) => unit = "";
  [@bs.get] external data: imageData => array(int) = "";
};

module Window = {
  type callbackId;
  [@bs.val] external requestAnimationFrame: (unit => unit) => callbackId = "";
};

module Document = {
  type dom;
  [@bs.scope "document"] [@bs.val]
  external querySelector: string => dom = "querySelector";

  [@bs.set] external textContent: (dom, string) => unit = "textContent";
};

[@bs.scope "Date"] [@bs.val] external now: unit => float = "now";
