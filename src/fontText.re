module BMFont = SdfFontBMFont;

type align =
  | Left
  | Center
  | Right;

type block = {
  align,
  font: option(string),
  height: option(float),
  color: option(Color.t),
  spacing: option(float),
  children: list(part)
}
and styled = {
  font: option(string),
  height: option(float),
  color: option(Color.t),
  spacing: option(float),
  children: list(part)
}
and part =
  | Text(string)
  | Styled(styled);

let block = (
  ~align=Left,
  ~font=?,
  ~height=?,
  ~color=?,
  ~spacing=?,
  ~children=[],
  ()
) => {
  {
    align,
    font,
    height,
    color,
    spacing,
    children
  }
};

let styled = (
  ~font=?,
  ~height=?,
  ~color=?,
  ~spacing=?,
  ~children=[],
  ()
) => {
  Styled({
    font,
    height,
    color,
    spacing,
    children
  })
};

let text = (text) => Text(text);

let styledText = (
  ~font=?,
  ~height=?,
  ~color=?,
  textString
) => {
  styled(
    ~font=?font,
    ~height=?height,
    ~color=?color,
    ~children=[
      text(textString)
    ]
  )
};

type calcStyle = {
  font: BMFont.bmFont,
  height: float,
  spacing: float,
  adjSpacing: float, /* Spacing adjusted with height */
  color: Color.t,
  align
};

type glyphData = {
  mutable x: float,
  mutable y: float,
  mutable w: float,
  mutable h: float,
  mutable color: Color.t,
  mutable glChar: BMFont.glChar,
  mutable code: int
};

module GlyphArrayB = ArrayB.Make({
  type a = glyphData;

  let nullChar : BMFont.glChar = {
    tx: 0.0,
    ty: 0.0,
    tx2: 0.0,
    ty2: 0.0,
    vw: 0.0,
    vh: 0.0,
    xOffset: 0.0,
    yOffset: 0.0,
    xAdvance: 0.0
  };

  /* Mutable default value with placeholder
     reference to glChar */
  let defaultVal = () : glyphData => {
    x: 0.0,
    y: 0.0,
    w: 0.0,
    h: 0.0,
    color: Color.fromFloats(1.0, 1.0, 1.0),
    glChar: nullChar,
    code: 0
  };
  let isStaticVal = false;
});

type layoutState = {
  /* Last glyph used to determine kerning */
  mutable lastGlyph: option(int),
  /* Current x position */
  mutable penX: float,
  /* Current y position */
  mutable penY: float,
  /* Total num chars */
  mutable iGlyph: int,
  /* Current line start */
  mutable glyphLineStart: int
};

module Layout = {
  /* Globally usable data for layout */
  type t = {
    fontStore: FontStore.t,
    glyphB: GlyphArrayB.t,
    defaultStyle: calcStyle
  };

  let make = (fontStore : FontStore.t) : t => {
    let font = Hashtbl.find(fontStore.fonts, "digitalt");
    {
      fontStore,
      glyphB: GlyphArrayB.make(100),
      defaultStyle: {
        font,
        height: 0.2,
        spacing: 0.0,
        adjSpacing: 0.0,
        color: Color.fromFloats(1.0, 1.0, 1.0),
        align: Left
      }
    }
  };

  let getVertices = (layout, numGlyphs, yScale, multicolor) => {
    let glyphB = layout.glyphB.data;
    /* Four points per glyph, each point has
       x, y, uvx, uvy. Multicolor has 3 more per point
       for color */
    let perGlyph = (multicolor) ? 16 : 28;
    let d = Array.make(numGlyphs * perGlyph, 0.0);
    let rec processGlyph = (iGlyph, i) => {
      if (iGlyph < numGlyphs) {
        let g = glyphB[iGlyph];
        let gl = g.glChar;
        /* todo: better indexing than quads */
        /* Bottom left */
        d[i] = g.x;
        d[i + 1] = g.y -. g.h;
        d[i + 2] = gl.tx;
        d[i + 3] = gl.ty2;
        /* Bottom right */
        d[i + 4] = g.x +. g.w;
        d[i + 5] = g.y -. g.h;
        d[i + 6] = gl.tx2;
        d[i + 7] = gl.ty2;
        /* Top right */
        d[i + 4] = g.x +. g.w;
        d[i + 5] = g.y;
        d[i + 6] = gl.tx2;
        d[i + 7] = gl.ty;
        /* Top left */
        d[i + 4] = g.x;
        d[i + 5] = g.y;
        d[i + 6] = gl.tx;
        d[i + 7] = gl.ty;
        /* Recurse next glyph */
        processGlyph(iGlyph + 1, i + perGlyph);
      };
    };
    processGlyph(0, 0);
    d
  };

  let layoutBlock = (layout, block : block, yScale) => {
    let curStyle = layout.defaultStyle;
    let lineStart = -1.0;
    let lineEnd = 1.0;
    /* State while doing layout */
    /* Todo: Use font baseline */
    let s = {
      lastGlyph: None,
      penX: 0.0,
      penY: 0.0,
      iGlyph: 0,
      glyphLineStart: 0
    };
    let glyphB = layout.glyphB.data;
    let spaceCode = Char.code(' ');
    let nlCode = Char.code('\n');
    /* Move characters in range in x direction */
    let rec moveX = (delta, i, until) => {
      glyphB[i].x = glyphB[i].x +. delta;
      if (i <= until) {
        moveX(delta, i + 1, until)
      }
    };
    /* Move characters in range in x and y direction */
    let rec moveXY = (deltaX, deltaY, i, until) => {
      glyphB[i].x = glyphB[i].x +. deltaX;
      glyphB[i].y = glyphB[i].y +. deltaY;
      if (i <= until) {
        moveXY(deltaX, deltaY, i + 1, until)
      }
    };
    /* Loops parts and add result to glyphBuffer */
    /* Lots of dangerous mutable code and bad microoptimizations
       The state and glyph buffer can be shared between
       layout calls to avoid allocations. One could also
       create own layout object maybe to better support
       cases like editable text or other text that changes */
    let rec layoutParts = (parts, curStyle) => {
      switch parts {
      | [] => ()
      | [part, ...rest] =>
        switch part {
        | Text(text) =>
          let textLen = String.length(text);
          GlyphArrayB.ensureSize(layout.glyphB, s.iGlyph + textLen);
          let glScale = curStyle.font.common.glScale;
          /* First non whitespace index from i and backwards */
          let rec nonWhitespace = (i, until) => {
            if (i <= until) {
              None
            } else if (glyphB[i].code == spaceCode) {
              nonWhitespace(i - 1, until)
            } else {
              Some(i)
            }
          };
          let truncateWhitespace = (fromIndex) => {
            /* Truncate between nonWhitespace and fromIndex
               by moving glyphs backwards and placing the
               truncated glyphs after */
            let rec collectTruncated = (i, until, list) => {
              if (i < until) {
                collectTruncated(i + 1, until, [glyphB[i], ...list])
              } else {
                list
              }
            };
            let rec moveBack = (i, until, truncateNum) => {
              if (i <= until) {
                glyphB[i] = glyphB[i + truncateNum];
                moveBack(i + 1, until, truncateNum);
              };
            };
            let truncateFrom = switch (nonWhitespace(fromIndex, s.glyphLineStart)) {
            | Some(nonWhitespace) => nonWhitespace
            | None => s.glyphLineStart
            };
            let numTruncated = fromIndex - truncateFrom;
            if (numTruncated > 0) {
              let truncated = collectTruncated(truncateFrom, fromIndex, []);
              moveBack(truncateFrom + 1, fromIndex - numTruncated, numTruncated);
              List.iteri((i, truncated) => {
                glyphB[fromIndex - i] = truncated;
              }, truncated);
            };
            numTruncated
          };
          let alignLine = (lastIndex) => {
            let lastGlyph = glyphB[lastIndex];
            switch (curStyle.align) {
            | Left => ()
            | Center =>
              let xAdj = (lineEnd -. lastGlyph.x +. lastGlyph.w) /. 2.0;
              moveX(xAdj, s.glyphLineStart, lastIndex);
            | Right =>
              let xAdj = lineEnd -. s.penX;
              moveX(xAdj, s.glyphLineStart, lastIndex);
            };
          };
          /* Loop for character in text */
          let rec addChar = (iText) => {
            if (iText >= textLen) {
              /* Reached end of text */
              alignLine(s.iGlyph);
            } else {
              let code = Char.code(text.[iText]);
              /* Check for newline char */
              if (code == nlCode) {
                /* Move back to first non whitespace */
                switch (nonWhitespace(s.iGlyph, s.glyphLineStart)) {
                | Some(iGlyph) =>
                  s.iGlyph = iGlyph;
                  if (iText + 1 < textLen) {
                    /* If more text, go to next line */
                    s.penY = s.penY +. curStyle.height;
                    s.glyphLineStart = iGlyph + 1;
                    s.penX = lineStart;
                    addChar(iText + 1);
                  };
                | None => 
                  /* No non-whitespace found.. Assume the user wants a newline */
                  if (iText + 1 < textLen) {
                    s.penY = s.penY +. curStyle.height;
                    s.glyphLineStart = s.iGlyph + 1;
                    s.penX = lineStart;
                    addChar(iText + 1);
                  };
                };
              } else {
                /* Normal character */
                let glChar =
                  switch curStyle.font.glChars[code] {
                  | Some(glChar) => glChar
                  | None => BMFont.addGlChar(curStyle.font, code)
                  };
                /* Kerning and spacing */
                switch s.lastGlyph {
                | Some(lastGlyph) =>
                  /* Kerning is not scaled to gl */
                  s.penX =
                    s.penX
                    +. curStyle.adjSpacing
                    +. float_of_int(BMFont.getKerning(curStyle.font, lastGlyph, code))
                    *. glScale
                    *. curStyle.height;
                | None =>
                  s.penX =
                    s.penX
                    +. curStyle.adjSpacing;
                };
                /* Check if we surpass max width */
                let nextPen = s.penX +. (glChar.xAdvance *. curStyle.height);
                let nextWidth = s.penX +. (glChar.vw *. curStyle.height);
                let iText = if (nextWidth >= lineEnd || nextPen >= lineEnd) {
                  /* Next char surpassed line width, attempt to find
                    whitespace to break on, or break after current char */
                  let rec prevSpace = (i) => {
                    if (i <= s.glyphLineStart) {
                      None
                    } else if (glyphB[i].code == spaceCode) {
                      Some(i)
                    } else {
                      prevSpace(i)
                    }
                  };
                  /* Skip whitespaces from text position when start of line */
                  let rec firstNonWhitespace = (iText) => {
                    if (iText >= textLen) {
                      None
                    } else if (text.[iText] == ' ') {
                      firstNonWhitespace(iText + 1)
                    } else {
                      Some(iText)
                    }
                  };
                  switch (prevSpace(s.iGlyph)) {
                  | Some(spaceIndex) =>
                    /* Especially for right aligned, we need to truncate whitespace */
                    let truncated = truncateWhitespace(spaceIndex);
                    let spaceIndex = spaceIndex - truncated;
                    /* Space found, move characters after to next line */
                    let subtractX = glyphB[spaceIndex + 1].x;
                    /* todo: Need to resolve current height for next line
                      based on moved characters */
                    let addY = curStyle.height;
                    moveXY(subtractX, addY, spaceIndex + 1, s.iGlyph);
                    /* Line is breaked, adjust positions according to align */
                    alignLine(spaceIndex - 1);
                    s.iGlyph = s.iGlyph - truncated;
                    s.penX = glyphB[s.iGlyph].x;
                    s.penY = glyphB[s.iGlyph].y;
                    firstNonWhitespace(iText)
                  | None =>
                    /* No space found, break after current char */
                    alignLine(s.iGlyph);
                    /* Prepare newline when there is another non whitespace character */
                    switch (firstNonWhitespace(iText)) {
                    | Some(_) as iText =>
                      s.penY = s.penY +. curStyle.height;
                      s.penX = lineStart;
                      s.glyphLineStart = s.iGlyph + 1;
                      iText
                    | None => None
                    }
                  }
                } else {
                  /* Within line, add as normal */
                  /* Increment posX */
                  s.penX = nextPen;
                  Some(iText)
                };
                switch (iText) {
                | Some(iText) =>
                  /* Set glyph data */
                  s.iGlyph = s.iGlyph + 1;
                  let glyph = layout.glyphB.data[s.iGlyph];
                  glyph.x = s.penX +. glChar.xOffset *. curStyle.height;
                  glyph.y = s.penY -. glChar.yOffset *. curStyle.height;
                  glyph.w = glChar.vw *. curStyle.height;
                  glyph.h = glChar.vh *. curStyle.height;
                  glyph.code = code;
                  glyph.color = curStyle.color;
                  glyph.glChar = glChar;
                  /* Recurse rest of chars in text */
                  if (iText < textLen) {
                    addChar(iText + 1);
                  };
                | None => ()
                };
              };
            };
          };
          /* Initiate char loop */
          addChar(0);
        | Styled(styled) =>
          let height = switch styled.height {
          | None => curStyle.height
          | Some(height) => height
          };
          let spacing = switch styled.spacing {
          | None => curStyle.spacing
          | Some(spacing) => spacing
          };
          let adjSpacing = spacing *. height;
          let font = switch styled.font {
          | None => curStyle.font
          | Some(font) => Hashtbl.find(layout.fontStore.fonts, font)
          };
          let newStyle = {
            font,
            height,
            spacing,
            adjSpacing,
            color: switch styled.color {
            | None => curStyle.color
            | Some(color) => color
            },
            align: curStyle.align
          };
          layoutParts(styled.children, newStyle);
        };
        layoutParts(rest, curStyle);
      };
    };
    layoutParts(block.children, curStyle);
  };
};
