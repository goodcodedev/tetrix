type style = {
  font: string,
  height: float,
  align: SdfFont.TextLayout.align,
  color: Color.t
};

type t = {
  text: string,
  style: style,
  next: option(t)
};

let makeStyle = (
  font,
  ~height=0.2,
  ~align=SdfFont.TextLayout.AlignLeft,
  ~color=Color.fromFloats(1.0, 1.0, 1.0),
  ()
) => {
  font,
  height,
  align,
  color
};

let makeText = (
  text,
  style,
  ~next=?,
  ()
) => {
  text,
  style,
  next
};