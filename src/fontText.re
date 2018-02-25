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
  mutable color: Color.t,
  mutable glChar: BMFont.glChar
};

module GlyphArrayB = ArrayB.Make({
  type a = glyphData;

  let nullChar : BMFont.glChar = {
    code: 0,
    x: 0.0,
    y: 0.0,
    tw: 0.0,
    th: 0.0,
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
    color: Color.fromFloats(1.0, 1.0, 1.0),
    glChar: nullChar
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
  mutable textLineStart: int,
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

  let layoutBlock = (self, block : block, yScale) => {
    let curStyle = self.defaultStyle;
    let lineStart = -1.0;
    let lineEnd = 1.0;
    /* State while doing layout */
    /* Todo: Use font baseline */
    let s = {
      lastGlyph: None,
      penX: 0.0,
      penY: 0.0,
      iGlyph: 0,
      textLineStart: 0,
      glyphLineStart: 0
    };
    let glyphB = self.glyphB.data;
    let spaceCode = Char.code(' ');
    let nlCode = Char.code('\n');
    /* Move characters in range in x direction */
    let rec moveX = (delta, i, until) => {
      glyphB[i].x = glyphB[i].x +. delta;
      if (i < until) {
        moveX(delta, i + 1, until)
      }
    };
    /* Move characters in range in x and y direction */
    let rec moveXY = (deltaX, deltaY, i, until) => {
      glyphB[i].x = glyphB[i].x +. deltaX;
      glyphB[i].y = glyphB[i].y +. deltaY;
      if (i < until) {
        moveXY(deltaX, deltaY, i + 1, until)
      }
    };
    /* First non whitespace index from i and backwards */
    let rec nonWhitespace = (i, until) => {
      if (i <= until) {
        None
      } else if (glyphB[i].glChar.code == spaceCode) {
        nonWhitespace(i - 1, until)
      } else {
        Some(i)
      }
    };
    /* Loops parts and add result to glyphBuffer */
    let rec layoutParts = (parts, curStyle) => {
      switch parts {
      | [] => ()
      | [part, ...rest] =>
        switch part {
        | Text(text) =>
          let textLen = String.length(text);
          GlyphArrayB.ensureSize(self.glyphB, s.iGlyph + textLen);
          let glScale = curStyle.font.common.glScale;
          /* Loop for character in text
             Differentiating between index of text and index of glyph
             since newlines don't go in glyphs and whitespaces
             might be truncated */
          let rec addChar = (iText, iGlyph) => {
            let code = Char.code(text.[iText]);
            /* Check for newline char */
            if (code == nlCode) {
              s.penX = lineStart;
              /* Recurse with iGlyph from non whitespace */
              switch (nonWhitespace(iGlyph, s.glyphLineStart)) {
              | Some(iGlyph) =>
                if (iText < textLen) {
                  s.penY = s.penY +. curStyle.height;
                  s.textLineStart = s.textLineStart + iText;
                  s.glyphLineStart = s.glyphLineStart + iGlyph;
                  addChar(iText + 1, iGlyph);
                };
              | None => 
                /* No non-whitespace found.. */
                if (iText < textLen) {
                  s.textLineStart = s.textLineStart + iText;
                  s.glyphLineStart = s.glyphLineStart + iGlyph;
                  addChar(iText + 1, iGlyph);
                };
              };
            } else {
              let glChar =
                switch curStyle.font.glChars[code] {
                | Some(glChar) => glChar
                | None => BMFont.addGlChar(curStyle.font, code)
                };
              /* Add kerning */
              switch s.lastGlyph {
              | Some(lastGlyph) =>
                s.penX = s.penX +. float_of_int(BMFont.getKerning(curStyle.font, lastGlyph, code)) *. glScale;
              | None => ()
              };
              /* Set glyph data */
              let glyph = self.glyphB.data[s.iGlyph + iGlyph];
              glyph.x = s.penX;
              glyph.y = s.penY;
              glyph.color = curStyle.color;
              glyph.glChar = glChar;
              let nextPen = s.penX +. glChar.xAdvance +. curStyle.adjSpacing;
              let nextWidth = s.penX +. glChar.vw;
              /* Check if we surpass max width */
              if (nextWidth >= lineEnd || nextPen >= lineEnd) {
                /* Next char surpassed line width, attempt to find
                  whitespace to break on, or break after current char */
                let rec prevSpace = (i) => {
                  if (i <= s.glyphLineStart) {
                    None
                  } else if (self.glyphB.data[i].glChar.code == spaceCode) {
                    Some(i)
                  } else {
                    prevSpace(i)
                  }
                };
                let lastLineChar =
                  switch (prevSpace(s.iGlyph + iGlyph)) {
                  | Some(spaceIndex) =>
                    switch (nonWhitespace(spaceIndex - 1, s.glyphLineStart)) {
                    | Some(nonWhitespace) =>
                      /* This will be last character on line */
                      let truncateNum = spaceIndex - nonWhitespace;
                      if (truncateNum > 0) {
                        /* Truncate between nonWhitespace and spaceIndex,
                          move truncated objects after to avoid mutation
                          on moved items */
                        let rec collectTruncated = (i, until, list) => {
                          if (i < until) {
                            collectTruncated(i + 1, until, [glyphB[i], ...list])
                          } else {
                            list
                          }
                        };
                        let truncated = collectTruncated(nonWhitespace, spaceIndex, []);
                        let rec truncate = (i, until, truncateNum) => {
                          if (i < until) {
                            glyphB[i] = glyphB[i + truncateNum];
                            truncate(i + 1, until, truncateNum);
                          };
                        };
                        truncate(nonWhitespace, spaceIndex, truncateNum);
                        List.iteri((i, truncated) => {
                          glyphB[spaceIndex + i] = truncated;
                        }, truncated);
                      };
                      /* Move characters after to next line */
                      let subtractX = glyphB[nonWhitespace].x +. glyphB[nonWhitespace].glChar.xAdvance;
                      let rec moveChars = (i) => {
                        self.glyphB.data[i].x = self.glyphB.data[i].x -. subtractX;
                        /* todo: Need to resolve current height for next line
                          based on moved characters */
                        self.glyphB.data[i].y = self.glyphB.data[i].y +. curStyle.height;
                        if (i < iGlyph) {
                          moveChars(i + 1);
                        };
                      };
                      /* todo: whitespace truncate */
                      moveChars(spaceIndex);
                      s.penY = s.penY +. curStyle.height;
                      s.penX = self.glyphB.data[iGlyph].x;
                      nonWhitespace
                    | None =>
                      /* Whole line is whitepace */
                      s.glyphLineStart
                    }
                  | None =>
                    /* No space found, break on prev char */
                    s.penY = s.penY +. curStyle.height;
                    s.penX = self.glyphB.data[iGlyph].x;
                    iGlyph
                  };
                /* Line is breaked, adjust positions according to align */
                switch (curStyle.align) {
                | Left => ()
                | Center =>
                  let moveX = (lineEnd -. s.penX) /. 2.0;
                  let rec moveCenter = (i) => {
                    self.glyphB.data[i].x = self.glyphB.data[i].x +. moveX;
                    if (i < lastLineChar) {
                      moveCenter(i + 1)
                    }
                  };
                  moveCenter(s.glyphLineStart);
                | Right =>
                  let moveX = lineEnd -. s.penX;
                  let rec moveCenter = (i) => {
                    self.glyphB.data[i].x = self.glyphB.data[i].x +. moveX;
                    if (i < lastLineChar) {
                      moveCenter(i + 1)
                    }
                  };
                  moveCenter(s.glyphLineStart);
                };
              } else {
                /* Increment posX */
                s.penX = nextPen;
                /* Recurse rest of chars in text */
                if (iText < textLen) {
                  addChar(iText + 1, iGlyph + 1);
                };
              };
            };
          };
          /* Initiate char loop */
          addChar(0, 0);
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
          | Some(font) => Hashtbl.find(self.fontStore.fonts, font)
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
