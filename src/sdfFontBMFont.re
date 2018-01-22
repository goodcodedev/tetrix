/* https://github.com/Jam3/load-bmfont */
/* https://github.com/Jam3/parse-bmfont-binary */
type bmChar = {
    id: int,
    x: int,
    y: int,
    width: int,
    height: int,
    xOffset: int,
    yOffset: int,
    xAdvance: int,
    page: int,
    channel: int
};

type bmInfo = {
    size: int,
    smooth: bool,
    unicode: bool,
    italic: bool,
    bold: bool,
    fixedHeight: bool,
    charset: int,
    stretchH: int,
    aa: int,
    padding: array(int),
    spacing: array(int),
    outline: int,
    face: string
};

type bmKerning = {
    first: int,
    second: int,
    amount: int
};

type bmCommon = {
    lineHeight: int,
    base: int,
    scaleW: int,
    scaleH: int,
    pages: int,
    packed: int,
    alphaChnl: int,
    redChnl: int,
    greenChnl: int,
    blueChnl: int
};

type bmFont = {
    kernings: list(bmKerning),
    info: bmInfo,
    common: bmCommon,
    pages: list(string),
    chars: list(bmChar),
    charsById: array(option(bmChar))
};

module Buffer = SdfFontBinaryBuffer;

let readInfo = (buf, i) : bmInfo => {
    let bitField = Buffer.readUInt8(buf, i + 2);
    {
        size: Buffer.readInt16LE(buf, i),
        smooth: ((bitField asr 7) land 1) == 1,
        unicode: ((bitField asr 6) land 1) == 1,
        italic: ((bitField asr 5) land 1) == 1,
        bold: ((bitField asr 4) land 1) == 1,
        fixedHeight: ((bitField asr 3) land 1) == 1,
        charset: Buffer.readUInt8(buf, i + 3),
        stretchH: Buffer.readUInt16LE(buf, i + 4),
        aa: Buffer.readUInt8(buf, i + 6),
        padding: [|
            Buffer.readInt8(buf, i + 7),
            Buffer.readInt8(buf, i + 8),
            Buffer.readInt8(buf, i + 9),
            Buffer.readInt8(buf, i + 10)
        |],
        spacing: [|
            Buffer.readInt8(buf, i + 11),
            Buffer.readInt8(buf, i + 12)
        |],
        outline: Buffer.readUInt8(buf, i + 13),
        face: Buffer.readStringNT(buf, i + 14)
    }
};

let readCommon = (buf, i) : bmCommon => {
    /* Red and green go to same(?) */
    {
        lineHeight: Buffer.readUInt16LE(buf, i),
        base: Buffer.readUInt16LE(buf, i+2),
        scaleW: Buffer.readUInt16LE(buf, i+4),
        scaleH: Buffer.readUInt16LE(buf, i+6),
        pages: Buffer.readUInt16LE(buf, i+8),
        packed: 0,
        alphaChnl: Buffer.readUInt8(buf, i+11),
        redChnl: Buffer.readUInt8(buf, i+12),
        greenChnl: Buffer.readUInt8(buf, i+12),
        blueChnl: Buffer.readUInt8(buf, i+13)
    }
};

let readPages = (buf, i, size) => {
    let text = Buffer.readStringNT(buf, i);
    let len = String.length(text) + 1;
    let count = size / len;
    let rec getPages = (c, pages) => {
        if (c >= count) {
            pages
        } else {
            /* todo: utf8 decode */
            /*let page = Buffer.utf8Slice(buf, i + c*len, (i + c*len) + String.length(text));*/
            let page = Buffer.readStringSlice(buf, i + c*len, (i + c*len) + String.length(text));
            getPages(c + 1, [page, ...pages])
        }
    };
    List.rev(getPages(0, []))
};

let readChars = (buf, i, blockSize) => {
    let rec getChars = (c, chars) => {
        if (c >= blockSize / 20) {
            chars
        } else {
            let off = c * 20;
            let bmChar = {
                id: Buffer.readUInt32LE(buf, i + 0 + off),
                x: Buffer.readUInt16LE(buf, i + 4 + off),
                y: Buffer.readUInt16LE(buf, i + 6 + off),
                width: Buffer.readUInt16LE(buf, i + 8 + off),
                height: Buffer.readUInt16LE(buf, i + 10 + off),
                xOffset: Buffer.readInt16LE(buf, i + 12 + off),
                yOffset: Buffer.readInt16LE(buf, i + 14 + off),
                xAdvance: Buffer.readUInt16LE(buf, i + 16 + off),
                page: Buffer.readUInt8(buf, i + 18 + off),
                channel: Buffer.readUInt8(buf, i + 19 + off)
            };
            getChars(c + 1, [bmChar, ...chars])
        }
    };
    List.rev(getChars(0, []))
};

let readKernings = (buf, i, blockSize) => {
    let rec getKernings = (c, kernings) => {
        if (c >= blockSize / 10) {
            kernings
        } else {
            let off = c * 10;
            let kerning = {
                first: Buffer.readUInt32LE(buf, i + off),
                second: Buffer.readUInt32LE(buf, i + 4 + off),
                amount: Buffer.readInt16LE(buf, i + 8 + off)
            };
            getKernings(c + 1, [kerning, ...kernings])
        }
    };
    List.rev(getKernings(0, []))
};

type blockType =
  | InfoBlock(bmInfo)
  | CommonBlock(bmCommon)
  | PagesBlock(list(string))
  | CharsBlock(list(bmChar), array(option(bmChar)))
  | KerningsBlock(list(bmKerning))
  | NoneBlock;

let readBlock = (buf, i) => {
    if (i > Array.length(buf) - 1) {
        (0, NoneBlock)
    } else {
        let blockID = Buffer.readUInt8(buf, i);
        let blockSize = Buffer.readInt32LE(buf, i + 1);
        let blockIndex = i + 5;
        let blockType = switch (blockID) {
        | 1 => InfoBlock(readInfo(buf, blockIndex))
        | 2 => CommonBlock(readCommon(buf, blockIndex))
        | 3 => PagesBlock(readPages(buf, blockIndex, blockSize))
        | 4 =>
            let chars = readChars(buf, blockIndex, blockSize);
            let maxId = List.fold_left((max, char) => {
                if (char.id > max) {
                    char.id
                } else {
                    max
                }
            }, 0, chars);
            let charsById = Array.make(maxId + 1, None);
            List.iter((char) => {
                charsById[char.id] = Some(char);
            }, chars);
            CharsBlock(chars, charsById)
        | 5 => KerningsBlock(readKernings(buf, blockIndex, blockSize))
        | _ => NoneBlock
        };
        (blockSize + 5, blockType)
    }
};

type bmFontBuilding = {
    mutable kernings: option(list(bmKerning)),
    mutable info: option(bmInfo),
    mutable common: option(bmCommon),
    mutable pages: option(list(string)),
    mutable chars: option(list(bmChar)),
    mutable charsById: option(array(option(bmChar)))
};

let parse = (buf) : bmFont => {
    /* Verify header */
    if (Buffer.readUInt8(buf, 0) != 66 ||
        Buffer.readUInt8(buf, 1) != 77 ||
        Buffer.readUInt8(buf, 2) != 70) {
        failwith("Could not verify header");
    };
    /* Check version */
    if (Buffer.readUInt8(buf, 3) != 3) {
        failwith("Only version 3 supported");
    };
    let i = ref(4);
    let building : bmFontBuilding = {
        kernings: None,
        info: None,
        common: None,
        pages: None,
        chars: None,
        charsById: None
    };
    for (_j in 0 to 4) {
        let (read, blockType) = readBlock(buf, i^);
        switch (blockType) {
        | KerningsBlock(kernings) => building.kernings = Some(kernings);
        | InfoBlock(info) => building.info = Some(info);
        | CommonBlock(common) => building.common = Some(common);
        | PagesBlock(pages) => building.pages = Some(pages);
        | CharsBlock(chars, charsById) =>
            building.chars = Some(chars);
            building.charsById = Some(charsById);
        | NoneBlock => ()
        };
        i := i^ + read;
    };
    switch ((building.kernings, building.info,
            building.common, building.pages, building.chars,
            building.charsById)) {
        | (Some(kernings),
           Some(info),
           Some(common),
           Some(pages),
           Some(chars),
           Some(charsById)) => {
            kernings: kernings,
            info: info,
            common: common,
            pages: pages,
            chars: chars,
            charsById: charsById
        }
        | (None,
           Some(info),
           Some(common),
           Some(pages),
           Some(chars),
           Some(charsById)) => {
            kernings: [],
            info: info,
            common: common,
            pages: pages,
            chars: chars,
            charsById: charsById
        }
        | _ => failwith("Could not parse font");
    }
};

let getKerning = (font : bmFont, prevId, currId) => {
    let rec findKerning = (kernings) => {
        switch (kernings) {
        | [] => 0
        | [kerning, ...rest] =>
            if (kerning.first == prevId && kerning.second == currId) {
                kerning.amount
            } else {
                findKerning(rest)
            }
        }
    };
    findKerning(font.kernings)
};