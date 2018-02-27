module BMFont = SdfFontBMFont;

type align =
  | Left
  | Center
  | Right;

type block = {
  align: option(align),
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

let defaultFont = "digitalt";
let defaultHeight = 0.2;
let defaultColor = Color.fromFloats(1.0, 1.0, 1.0);

let block = (
  ~align=?,
  ~font=?,
  ~height=?,
  ~color=?,
  ~spacing=?,
  children
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
  children
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
    [
      text(textString)
    ]
  )
};

type blockInfo = {
  fonts: list(string),
  colors: list(Color.t)
};

/* Block info can be used to determine required
   fonts and colors */
let getBlockInfo = (block : block) => {
  let collectInfo = (info, font, color) => {
    let info = switch font {
    | Some(font) => {...info, fonts: [font, ...info.fonts]}
    | None => info
    };
    switch color {
    | Some(color) => {...info, colors: [color, ...info.colors]}
    | None => info
    };
  };
  let rec processBlock = (block, info) => {
    let info = collectInfo(info, block.font, block.color);
    List.fold_left(
      (info, child) => {
        switch child {
        | Text(_) => info
        | Styled(styled) => processStyled(styled, info)
        }
      },
      info,
      block.children
    )
  }
  and processStyled = (styled, info) => {
    let info = collectInfo(info, styled.font, styled.color);
    List.fold_left(
      (info, child) => {
        switch child {
        | Text(_) => info
        | Styled(styled) => processStyled(styled, info)
        }
      },
      info,
      block.children
    )
  };
  let info = processBlock(block, {colors: [], fonts: []});
  /* If no fonts, add default font */
  let info = if (List.length(info.fonts) == 0) {
    {...info, fonts: [defaultFont]}
  } else {
    {...info, fonts: List.sort_uniq(String.compare, info.fonts)}
  };
  /* If no colors, add default color */
  if (List.length(info.colors) == 0) {
    {...info, colors: [defaultColor]}
  } else {
    {...info, colors: List.sort_uniq(Color.compare, info.colors)}
  }
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
  /* Where line ends on y axis */
  mutable yLineEnd: float,
  /* Total num chars */
  mutable iGlyph: int,
  /* Current line start */
  mutable glyphLineStart: int
};

module FontLayout = {
  /* Globally usable data for layout */
  type t = {
    store: FontStore.t,
    glyphB: GlyphArrayB.t
  };

  let make = (store : FontStore.t) : t => {
    {
      store,
      glyphB: GlyphArrayB.make(100),
    }
  };

  let getVertices = (layout, numGlyphs, multicolor) => {
    let glyphB = layout.glyphB.data;
    /* Four points per glyph, each point has
       x, y, uvx, uvy. Multicolor has 3 more per point
       for color */
    let perGlyph = (multicolor) ? 28 : 16;
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
        d[i + 8] = g.x +. g.w;
        d[i + 9] = g.y;
        d[i + 10] = gl.tx2;
        d[i + 11] = gl.ty;
        /* Top left */
        d[i + 12] = g.x;
        d[i + 13] = g.y;
        d[i + 14] = gl.tx;
        d[i + 15] = gl.ty;
        /* Recurse next glyph */
        processGlyph(iGlyph + 1, i + perGlyph);
      };
    };
    processGlyph(0, 0);
    d
  };

  let defaultStyle = (layout) => {
    let font = Hashtbl.find(layout.store.fonts, defaultFont);
    {
      font,
      height: defaultHeight,
      spacing: 0.0,
      adjSpacing: 0.0,
      color: defaultColor,
      align: Left
    }
  };

  /* Todo: Verify algorithms when we have better setup
     to "unit test"
     Would it be good for performance to split this up? */
  let layoutBlock = (layout, block : block) => {
    let curStyle = defaultStyle(layout);
    let lineStart = -1.0;
    let lineEnd = 1.0;
    /* State while doing layout */
    /* Todo: Use font baseline */
    let s = {
      lastGlyph: None,
      penX: lineStart,
      penY: 0.0,
      yLineEnd: 0.0,
      iGlyph: 0,
      glyphLineStart: 0
    };
    /* Some linespacing factor, lineHeight might be off somehow */
    let lineHeight = 2.0;
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
          Js.log("Processing text " ++ text);
          let textLen = String.length(text);
          GlyphArrayB.ensureSize(layout.glyphB, s.iGlyph + textLen);
          let glScale = curStyle.font.common.glScale;
          /* First non whitespace index from i and backwards */
          let rec nonWhitespace = (i, until) => {
            if (i < until) {
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
               truncated glyphs after current iGlyph */
            let rec collectTruncated = (i, until, list) => {
              if (i <= until) {
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
            | Some(nonWhitespace) => nonWhitespace + 1
            | None => s.glyphLineStart
            };
            let numTruncated = fromIndex - truncateFrom + 1;
            let numAfter = s.iGlyph - 1 - fromIndex;
            let numMove = min(numTruncated, numAfter);
            if (numMove > 0) {
              let truncated = collectTruncated(truncateFrom, truncateFrom + numMove, []);
              moveBack(truncateFrom, truncateFrom + numMove, numTruncated);
              /* Put truncated items where items where moved from */
              List.iteri((i, truncated) => {
                glyphB[truncateFrom + numTruncated + i] = truncated;
              }, truncated);
            };
            numTruncated
          };
          let alignLine = (lastIndex) => {
            let lastGlyph = glyphB[lastIndex];
            Js.log2("Aligning " ++ text, String.make(1, Char.chr(lastGlyph.code)));
            switch (curStyle.align) {
            | Left => ()
            | Center =>
              let xAdj = (lineEnd -. lastGlyph.x -. lastGlyph.w) /. 2.0;
              moveX(xAdj, s.glyphLineStart, lastIndex);
            | Right =>
              let xAdj = lineEnd -. s.penX;
              moveX(xAdj, s.glyphLineStart, lastIndex);
            };
          };
                  let rec printBuffer = (i) => {
                    if (i < s.iGlyph) {
                      Js.log(String.make(1, Char.chr(glyphB[i].code)));
                      printBuffer(i + 1);
                    };
                  };
          /* Loop for character in text */
          let rec addChar = (iText) => {
            if (iText >= textLen) {
              /* Reached end of text */
              alignLine(s.iGlyph - 1);
            } else {
              let code = Char.code(text.[iText]);
              Js.log("Processing char " ++ String.make(1, Char.chr(code)));
              /* Check for newline char */
              if (code == nlCode) {
                /* Move back to first non whitespace */
                switch (nonWhitespace(s.iGlyph - 1, s.glyphLineStart)) {
                | Some(iGlyph) =>
                  alignLine(iGlyph);
                  s.iGlyph = iGlyph + 1;
                  if (iText + 1 < textLen) {
                    /* If more text, go to next line */
                    s.penY = s.penY -. curStyle.height *. lineHeight;
                    s.yLineEnd = s.yLineEnd -. curStyle.height *. lineHeight;
                    s.glyphLineStart = iGlyph + 1;
                    s.penX = lineStart;
                    addChar(iText + 1);
                  };
                | None => 
                  /* No non-whitespace found.. Assume the user wants a newline */
                  if (iText + 1 < textLen) {
                    s.penY = s.penY -. curStyle.height *. lineHeight;
                    s.yLineEnd = s.yLineEnd -. curStyle.height *. lineHeight;
                    s.glyphLineStart = s.iGlyph;
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
                let iText = if (s.penX +. (glChar.vw *. curStyle.height) > lineEnd) {
                  /* Next char surpassed line width, attempt to find
                    whitespace to break on, or break after current char */
                  let rec prevSpace = (i) => {
                    if (i <= s.glyphLineStart) {
                      None
                    } else if (glyphB[i].code == spaceCode) {
                      Some(i)
                    } else {
                      prevSpace(i - 1)
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
                  switch (prevSpace(s.iGlyph - 1)) {
                  | Some(spaceIndex) =>
                    Js.log2("Space at", spaceIndex);
                    Js.log("Before truncate");
                    printBuffer(0);
                    /* Especially for right aligned, we need to truncate whitespace
                       Will also reduce vertex data */
                    let truncated = truncateWhitespace(spaceIndex);
                    s.iGlyph = s.iGlyph - truncated;
                    Js.log("After truncate");
                    printBuffer(0);
                    let spaceIndex = spaceIndex - truncated;
                    [%debugger];
                    /* Adding break, adjust positions according to align */
                    alignLine(spaceIndex);
                    Js.log3("Spaceindex", spaceIndex, s.iGlyph);
                    if (s.iGlyph - 1 > spaceIndex) {
                      /* There are characters after space */
                      /* Move characters after to next line */
                      let subtractX = glyphB[spaceIndex + 1].x *. (-1.0);
                      /* todo: Need to resolve current height for next line
                        based on moved characters */
                      let addY = curStyle.height *. lineHeight *. -1.0;
                      moveXY(subtractX, addY, spaceIndex + 1, s.iGlyph - 1);
                      s.penX = glyphB[s.iGlyph - 1].x;
                      s.penY = glyphB[s.iGlyph - 1].y;
                      s.yLineEnd = s.yLineEnd -. curStyle.height *. lineHeight;
                    } else {
                      s.penX = lineStart;
                      s.penY = s.penY -. curStyle.height *. lineHeight;
                      s.yLineEnd = s.yLineEnd -. curStyle.height *. lineHeight;
                    };
                    firstNonWhitespace(iText)
                  | None =>
                    /* No space found, break after current char */
                    alignLine(s.iGlyph - 1);
                    /* Prepare newline when there is another non whitespace character */
                    switch (firstNonWhitespace(iText)) {
                    | Some(_) as iText =>
                      s.penY = s.penY -. curStyle.height *. lineHeight;
                      s.yLineEnd = s.yLineEnd -. curStyle.height *. lineHeight;
                      s.penX = lineStart;
                      s.glyphLineStart = s.iGlyph;
                      iText
                    | None => None
                    }
                  }
                } else {
                  /* Within line, add as normal */
                  Some(iText)
                };
                /* There may have been a break and then
                   no more non-whitespace */
                switch (iText) {
                | Some(iText) =>
                  /* Set glyph data */
                  let glyph = glyphB[s.iGlyph];
                  Js.log3("Pen x y", s.penX, s.penY);
                  Js.log2("Adding " ++ String.make(1, Char.chr(code)), s.iGlyph);
                  glyph.x = s.penX +. glChar.xOffset *. curStyle.height;
                  glyph.y = s.penY -. glChar.yOffset *. curStyle.height;
                  glyph.w = glChar.vw *. curStyle.height;
                  glyph.h = glChar.vh *. curStyle.height;
                  glyph.code = code;
                  glyph.color = curStyle.color;
                  glyph.glChar = glChar;
                  /* Recurse rest of chars in text */
                  if (iText < textLen) {
                    s.iGlyph = s.iGlyph + 1;
                    /* Increment posX */
                    s.penX = s.penX +. (glChar.xAdvance *. curStyle.height);
                    addChar(iText + 1);
                  };
                | None => ()
                };
              };
            };
          };
          /* Initiate char loop */
          addChar(0);
          Js.log("After addchar");
          printBuffer(0);
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
          | Some(font) => Hashtbl.find(layout.store.fonts, font)
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
    /* Set curStyle from block */
    let height = switch block.height {
    | None => curStyle.height
    | Some(height) => height
    };
    let spacing = switch block.spacing {
    | None => curStyle.spacing
    | Some(spacing) => spacing
    };
    let adjSpacing = spacing *. height;
    let font = switch block.font {
    | None => curStyle.font
    | Some(font) => Hashtbl.find(layout.store.fonts, font)
    };
    let curStyle = {
      font,
      height,
      spacing,
      adjSpacing,
      color: switch block.color {
      | None => curStyle.color
      | Some(color) => color
      },
      align: switch block.align {
      | None => curStyle.align
      | Some(align) => align
      }
    };
    /* Todo: Need to take baseline into consideration more places,
       when changing height/fonts. Some edge cases around newlines,
       how to handle based on previous lines font/height */
    s.yLineEnd = s.yLineEnd -. curStyle.height;
    s.penY = s.yLineEnd +. curStyle.font.common.glBase *. curStyle.height;
    Js.log3("Height, base", curStyle.height, curStyle.font.common.glBase *. curStyle.height);
    layoutParts(block.children, curStyle);
    (s.iGlyph, s.yLineEnd)
  };

  let layoutVertices = (layout, block) => {
    let (iGlyph, yLineEnd) = layoutBlock(layout, block);
    (getVertices(layout, iGlyph, false), yLineEnd)
  }
};
