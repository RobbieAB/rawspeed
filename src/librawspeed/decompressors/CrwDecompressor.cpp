/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2009-2014 Klaus Post
    Copyright (C) 2014 Pedro Côrte-Real
    Copyright (C) 2015-2018 Roman Lebedev

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

#include "decompressors/CrwDecompressor.h"
#include "common/Common.h"                // for uint32_t, uint8_t, uint16_t
#include "common/Point.h"                 // for iPoint2D
#include "common/RawImage.h"              // for RawImage, RawImageData
#include "decoders/RawDecoderException.h" // for ThrowRDE
#include "decompressors/HuffmanTable.h"   // for HuffmanTable
#include "io/BitPumpJPEG.h"               // for BitPumpJPEG, BitStream<>::...
#include "io/Buffer.h"                    // for Buffer
#include "io/ByteStream.h"                // for ByteStream
#include <array>                          // for array, array<>::value_type
#include <cassert>                        // for assert

using std::array;

namespace rawspeed {

CrwDecompressor::CrwDecompressor(const RawImage& img, uint32_t dec_table,
                                 bool lowbits_, ByteStream rawData)
    : mRaw(img), lowbits(lowbits_) {
  if (mRaw->getCpp() != 1 || mRaw->getDataType() != TYPE_USHORT16 ||
      mRaw->getBpp() != 2)
    ThrowRDE("Unexpected component count / data type");

  const uint32_t width = mRaw->dim.x;
  const uint32_t height = mRaw->dim.y;

  if (width == 0 || height == 0 || width % 4 != 0 || width > 4104 ||
      height > 3048 || (height * width) % 64 != 0)
    ThrowRDE("Unexpected image dimensions found: (%u; %u)", width, height);

  if (lowbits) {
    // If there are low bits, the first part (size is calculatable) is low bits
    // Each block is 4 pairs of 2 bits, so we have 1 block per 4 pixels
    const unsigned lBlocks = 1 * height * width / 4;
    assert(lBlocks > 0);
    lowbitInput = rawData.getStream(lBlocks);
  }

  // We always ignore next 514 bytes of 'padding'. No idea what is in there.
  rawData.skipBytes(514);

  // Rest is the high bits.
  rawInput = rawData.getStream(rawData.getRemainSize());

  mHuff = initHuffTables(dec_table);
}

HuffmanTable CrwDecompressor::makeDecoder(const uint8_t* ncpl,
                                          const uint8_t* values) {
  assert(ncpl);

  HuffmanTable ht;
  auto count = ht.setNCodesPerLength(Buffer(ncpl, 16));
  ht.setCodeValues(Buffer(values, count));
  ht.setup(false, false);

  return ht;
}

CrwDecompressor::crw_hts CrwDecompressor::initHuffTables(uint32_t table) {
  if (table > 2)
    ThrowRDE("Wrong table number: %u", table);

  // NCodesPerLength
  static const std::array<std::array<uint8_t, 16>, 3> first_tree_ncpl = {{
      {0, 1, 4, 2, 3, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0},
      {0, 2, 2, 3, 1, 1, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0},
      {0, 0, 6, 3, 1, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  }};

  static const std::array<std::array<uint8_t, 13>, 3> first_tree_len = {{
      {0x4, 0x3, 0x5, 0x6, 0x2, 0x7, 0x1, 0x8, 0x9, 0x0, 0xa, 0xb, 0xf},
      {0x3, 0x2, 0x4, 0x1, 0x5, 0x0, 0x6, 0x7, 0x9, 0x8, 0xa, 0xb, 0xf},
      {0x6, 0x5, 0x7, 0x4, 0x8, 0x3, 0x9, 0x2, 0x0, 0xa, 0x1, 0xb, 0xf},
  }};

  static const std::array<std::array<uint8_t, 13>, 3> first_tree_index = {{
      {0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xf},
      {0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xf},
      {0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xf},
  }};

  // NCodesPerLength
  static const std::array<std::array<uint8_t, 16>, 3> second_tree_ncpl = {{
      {0, 2, 2, 2, 1, 4, 2, 1, 2, 5, 1, 1, 0, 0, 0, 139},
      {0, 2, 2, 1, 4, 1, 4, 1, 3, 3, 1, 0, 0, 0, 0, 140},
      {0, 0, 6, 2, 1, 3, 3, 2, 5, 1, 2, 2, 8, 10, 0, 117},
  }};

  static const std::array<std::array<uint8_t, 164>, 3> second_tree_len = {{
      {0x3, 0x4, 0x2, 0x5, 0x1, 0x6, 0x7, 0x8, 0x2, 0x3, 0x1, 0x4, 0x9, 0x5,
       0x2, 0x0, 0x1, 0x6, 0xa, 0x0, 0x3, 0x7, 0x4, 0x1, 0x2, 0x8, 0x9, 0x3,
       0x5, 0x1, 0x4, 0x2, 0x5, 0x1, 0x6, 0x7, 0x8, 0x9, 0x9, 0x6, 0xa, 0x9,
       0x6, 0x7, 0x8, 0x7, 0x2, 0x5, 0x8, 0x3, 0x6, 0x9, 0x7, 0x4, 0x1, 0x9,
       0x1, 0x8, 0x5, 0x6, 0x7, 0x9, 0x7, 0x3, 0x7, 0x4, 0x6, 0x8, 0x7, 0x8,
       0x5, 0x9, 0x9, 0x1, 0xa, 0x8, 0x8, 0x5, 0x9, 0x6, 0x7, 0x8, 0x7, 0x6,
       0x5, 0x4, 0x9, 0x8, 0x1, 0x5, 0x6, 0x4, 0x8, 0x1, 0xa, 0x4, 0x2, 0x9,
       0x7, 0x6, 0x4, 0x5, 0xa, 0x7, 0x3, 0x9, 0x8, 0x6, 0x2, 0x7, 0x5, 0x8,
       0x9, 0x1, 0x4, 0x1, 0x9, 0xa, 0x2, 0x5, 0x6, 0x7, 0x3, 0x8, 0x1, 0x6,
       0xa, 0x4, 0x1, 0xa, 0xa, 0x6, 0x3, 0x1, 0x3, 0x5, 0xa, 0x2, 0xa, 0xa,
       0x4, 0x4, 0x3, 0x5, 0x5, 0x3, 0x2, 0x4, 0x2, 0xa, 0xa, 0x4, 0x2, 0xa,
       0x3, 0x3, 0x2, 0x3, 0xa, 0x2, 0x2, 0x3, 0xf, 0xf},
      {0x2, 0x3, 0x1, 0x4, 0x5, 0x2, 0x1, 0x6, 0x3, 0x7, 0x8, 0x4, 0x2, 0x9,
       0x1, 0x0, 0x3, 0x5, 0x1, 0x2, 0xa, 0x6, 0x0, 0x4, 0x3, 0x1, 0x2, 0x9,
       0x7, 0x5, 0x8, 0x1, 0x4, 0x3, 0x2, 0x9, 0x5, 0x1, 0x9, 0x1, 0x2, 0x6,
       0x3, 0x6, 0x8, 0xa, 0x7, 0x1, 0x7, 0x1, 0x9, 0x5, 0x5, 0x8, 0x2, 0x9,
       0x1, 0x1, 0x4, 0x9, 0x4, 0x8, 0x1, 0xa, 0x7, 0x1, 0x1, 0x9, 0x9, 0x7,
       0x3, 0xa, 0x9, 0x6, 0x6, 0x8, 0xa, 0xa, 0x8, 0x9, 0xa, 0x5, 0x4, 0x6,
       0x5, 0x1, 0x6, 0x6, 0x6, 0x6, 0x9, 0x5, 0x9, 0x5, 0x5, 0x4, 0x7, 0x7,
       0xa, 0x7, 0x8, 0x3, 0x7, 0x8, 0x9, 0x7, 0x7, 0xa, 0x8, 0x2, 0x4, 0xa,
       0x4, 0x6, 0x5, 0xa, 0x4, 0x4, 0x6, 0x2, 0x3, 0x8, 0x5, 0x8, 0x4, 0x5,
       0x6, 0x9, 0x2, 0x3, 0x3, 0x2, 0x6, 0x7, 0x3, 0xa, 0x4, 0x5, 0x7, 0x8,
       0x8, 0xa, 0x7, 0x7, 0x4, 0x4, 0x2, 0x8, 0x5, 0xa, 0xa, 0x8, 0x3, 0x6,
       0x9, 0x2, 0x3, 0x2, 0x2, 0x3, 0xa, 0x3, 0xf, 0xf},
      {0x4, 0x5, 0x3, 0x6, 0x2, 0x7, 0x1, 0x8, 0x9, 0x2, 0x3, 0x4, 0x1, 0x5,
       0xa, 0x6, 0x7, 0x0, 0x0, 0x2, 0x1, 0x8, 0x3, 0x9, 0x4, 0x2, 0x1, 0x5,
       0x3, 0x8, 0x7, 0x4, 0x5, 0x6, 0x9, 0x9, 0x7, 0x8, 0x9, 0x8, 0x6, 0x8,
       0x7, 0x1, 0x9, 0x7, 0x6, 0x2, 0x6, 0x9, 0xa, 0x5, 0x8, 0x7, 0x9, 0x8,
       0x4, 0x6, 0x9, 0x7, 0x7, 0x9, 0xa, 0x5, 0x8, 0x6, 0x7, 0x9, 0x9, 0x8,
       0x8, 0x2, 0x7, 0x8, 0x5, 0x4, 0x1, 0x6, 0x9, 0x8, 0xa, 0x6, 0x7, 0x5,
       0xa, 0x5, 0x5, 0x6, 0x6, 0x4, 0x9, 0x4, 0x3, 0xa, 0x8, 0x3, 0x5, 0x7,
       0x4, 0x6, 0x7, 0xa, 0x4, 0xa, 0x9, 0x8, 0x8, 0x7, 0xa, 0xa, 0x3, 0xa,
       0x1, 0x7, 0x4, 0x6, 0x5, 0x9, 0x2, 0x6, 0x1, 0x1, 0x3, 0x6, 0xa, 0x2,
       0x5, 0x2, 0x3, 0x5, 0x2, 0x4, 0x4, 0xa, 0x4, 0x5, 0x3, 0x2, 0x1, 0x5,
       0x3, 0xa, 0x4, 0xa, 0x2, 0x1, 0x4, 0x1, 0x3, 0x3, 0xa, 0x3, 0x2, 0x2,
       0x1, 0x3, 0x2, 0x1, 0x1, 0x3, 0x2, 0x1, 0xf, 0xf},
  }};

  static const std::array<std::array<uint8_t, 164>, 3> second_tree_index = {{
      {0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x1, 0x1, 0x1, 0x0, 0x1,
       0x2, 0x0, 0x2, 0x1, 0x0, 0xf, 0x2, 0x1, 0x2, 0x3, 0x3, 0x1, 0x1, 0x3,
       0x2, 0x4, 0x3, 0x4, 0x3, 0x5, 0x3, 0x3, 0x3, 0x2, 0x7, 0x2, 0x1, 0x3,
       0x5, 0x5, 0x2, 0x2, 0x5, 0x5, 0x5, 0x4, 0x7, 0x5, 0x7, 0x5, 0x6, 0xf,
       0x7, 0x7, 0x7, 0x9, 0x9, 0x4, 0xb, 0x5, 0xd, 0x7, 0xb, 0x9, 0x4, 0x4,
       0x9, 0x6, 0x9, 0x9, 0xf, 0xb, 0x6, 0xb, 0xb, 0xd, 0xf, 0xd, 0x6, 0x4,
       0x4, 0x9, 0x8, 0xf, 0x8, 0xd, 0xf, 0xb, 0x8, 0xb, 0x2, 0x4, 0x7, 0xd,
       0x8, 0x6, 0xd, 0xf, 0x3, 0xa, 0x7, 0xa, 0xa, 0x8, 0x6, 0xc, 0x6, 0xc,
       0xc, 0xa, 0xf, 0xd, 0xe, 0x5, 0x9, 0x8, 0xa, 0xe, 0x9, 0xe, 0xc, 0xc,
       0x7, 0x6, 0xe, 0x4, 0x6, 0xe, 0xb, 0xf, 0xd, 0xa, 0x8, 0xb, 0x9, 0xb,
       0x8, 0xa, 0x6, 0xe, 0xc, 0xf, 0xd, 0xc, 0x8, 0xa, 0xd, 0xe, 0xf, 0xc,
       0x8, 0xa, 0xa, 0xc, 0xe, 0xc, 0xe, 0xe, 0xf, 0xf},
      {0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x1, 0x0, 0x1, 0x0, 0x0, 0x1, 0x2, 0x0,
       0x2, 0x0, 0x2, 0x1, 0x3, 0x3, 0x0, 0x1, 0xf, 0x2, 0x3, 0x4, 0x4, 0x1,
       0x1, 0x2, 0x1, 0x5, 0x3, 0x4, 0x5, 0x2, 0x3, 0x6, 0x3, 0x7, 0x6, 0x3,
       0x5, 0x2, 0x3, 0x1, 0x3, 0x8, 0x2, 0x9, 0x7, 0x5, 0x4, 0x2, 0x7, 0x5,
       0xa, 0xb, 0x4, 0x6, 0x5, 0x5, 0xd, 0xf, 0x5, 0xe, 0xf, 0xb, 0x4, 0x4,
       0x6, 0x6, 0xf, 0x5, 0x4, 0xa, 0x2, 0x4, 0x7, 0x9, 0x3, 0x7, 0x7, 0x8,
       0x6, 0xc, 0x7, 0xb, 0x9, 0xd, 0x8, 0x8, 0xc, 0xf, 0x9, 0xb, 0xc, 0xf,
       0x8, 0x9, 0xb, 0x7, 0xb, 0xd, 0xd, 0x8, 0xa, 0x7, 0x4, 0x8, 0x8, 0xe,
       0xf, 0xa, 0xc, 0x5, 0x9, 0xa, 0xc, 0x9, 0xc, 0x6, 0xb, 0xc, 0xe, 0xe,
       0xe, 0xe, 0xa, 0xa, 0xe, 0xc, 0x6, 0x6, 0x9, 0xa, 0xd, 0xd, 0xe, 0xf,
       0x8, 0x9, 0xd, 0x7, 0xc, 0x6, 0xe, 0x9, 0xa, 0xc, 0xd, 0xe, 0xf, 0xf,
       0xa, 0xb, 0xb, 0xf, 0xd, 0x8, 0xb, 0xd, 0xf, 0xf},
      {0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1, 0x1, 0x1, 0x1, 0x1,
       0x0, 0x1, 0x1, 0xf, 0x0, 0x2, 0x2, 0x1, 0x2, 0x1, 0x2, 0x3, 0x3, 0x2,
       0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x3, 0x7, 0x5, 0x5, 0x5, 0x2, 0x5, 0x7,
       0x2, 0x4, 0x2, 0x7, 0x2, 0x4, 0x7, 0x9, 0x1, 0x5, 0x9, 0x9, 0xf, 0x4,
       0x5, 0x9, 0x8, 0x4, 0xb, 0x4, 0xf, 0x7, 0x6, 0xb, 0x6, 0x6, 0xb, 0xb,
       0xd, 0x5, 0xd, 0x8, 0xb, 0x7, 0x5, 0x4, 0xd, 0xf, 0x3, 0xd, 0x8, 0x4,
       0x7, 0x9, 0xd, 0xf, 0x8, 0xb, 0xa, 0x9, 0x5, 0x2, 0xa, 0x4, 0xf, 0xf,
       0xd, 0x6, 0xa, 0x5, 0x4, 0x8, 0xc, 0xe, 0xc, 0xe, 0x9, 0x6, 0x7, 0x4,
       0x6, 0xc, 0xf, 0xc, 0x6, 0xe, 0x7, 0xe, 0x7, 0x9, 0x9, 0xa, 0xd, 0x9,
       0x8, 0x6, 0xf, 0xc, 0xb, 0xa, 0x8, 0xb, 0x6, 0xa, 0xb, 0xd, 0x8, 0xe,
       0xd, 0xa, 0xc, 0xc, 0xf, 0xb, 0xe, 0xd, 0x8, 0x6, 0xe, 0xc, 0xe, 0x8,
       0xf, 0xa, 0xc, 0xa, 0xc, 0xe, 0xa, 0xe, 0xf, 0xf},
  }};

  array<array<HuffmanTable, 2>, 2> mHuff = {{
      {{makeDecoder(first_tree_ncpl[table].data(),
                    first_tree_len[table].data()),
        makeDecoder(first_tree_ncpl[table].data(),
                    first_tree_index[table].data())}},
      {{makeDecoder(second_tree_ncpl[table].data(),
                    second_tree_len[table].data()),
        makeDecoder(second_tree_ncpl[table].data(),
                    second_tree_index[table].data())}},
  }};

  return mHuff;
}

// FIXME: this function is horrible.
inline void CrwDecompressor::decodeBlock(std::array<int, 64>* diffBuf,
                                         const crw_hts& mHuff,
                                         BitPumpJPEG* lPump,
                                         BitPumpJPEG* iPump) {
  assert(diffBuf);
  assert(lPump);

  // decode the block
  for (int i = 0; i < 64; i++) {
    lPump->fill(32);
    iPump->fill(32);

    const int len = mHuff[i > 0][0].decodeCodeValue(*lPump);
    const int index = mHuff[i > 0][1].decodeCodeValue(*iPump);
    assert(len >= 0 && index >= 0);

    if (len == 0 && index == 0 && i)
      break;

    if (len == 0xf && index == 0xf)
      continue;

    i += index;

    if (len == 0)
      continue;

    int diff = lPump->getBitsNoFill(len);
    iPump->skipBitsNoFill(len);

    if (i >= 64)
      break;

    diff = HuffmanTable::extend(diff, len);

    (*diffBuf)[i] = diff;
  }
}

// FIXME: this function is horrible.
void CrwDecompressor::decompress() {
  const Array2DRef<uint16_t> out(mRaw->getU16DataAsUncroppedArray2DRef());
  assert(out.width > 0);
  assert(out.width % 4 == 0);
  assert(out.height > 0);

  {
    // Each block encodes 64 pixels

    assert((out.height * out.width) % 64 == 0);
    const unsigned hBlocks = out.height * out.width / 64;
    assert(hBlocks > 0);

    BitPumpJPEG lPump(rawInput);
    BitPumpJPEG iPump(rawInput);

    int carry = 0;
    std::array<int, 2> base = {512, 512}; // starting predictors

    int row = 0;
    int col = 0;

    for (unsigned block = 0; block < hBlocks; block++) {
      array<int, 64> diffBuf = {{}};
      decodeBlock(&diffBuf, mHuff, &lPump, &iPump);

      // predict and output the block

      diffBuf[0] += carry;
      carry = diffBuf[0];

      for (uint32_t k = 0; k < 64; ++k, ++col) {
        if (col == out.width) {
          // new line. sadly, does not always happen when k == 0.
          col = 0;
          row++;
          base = {512, 512}; // reinit.
        }

        base[k & 1] += diffBuf[k];

        if (!isIntN(base[k & 1], 10))
          ThrowRDE("Error decompressing");

        out(row, col) = base[k & 1];
      }
    }
    assert(row == (out.height - 1));
    assert(col == out.width);
  }

  // Add the uncompressed 2 low bits to the decoded 8 high bits
  if (lowbits) {
    for (int row = 0; row < out.height; row++) {
      for (int col = 0; col < out.width; /* NOTE: col += 4 */) {
        const uint8_t c = lowbitInput.getByte();
        // LSB-packed: p3 << 6 | p2 << 4 | p1 << 2 | p0 << 0

        // We have read 8 bits, which is 4 pairs of 2 bits. So process 4 pixels.
        for (uint32_t p = 0; p < 4; ++p, ++col) {
          uint16_t& pixel = out(row, col);

          uint16_t low = (c >> (2 * p)) & 0b11;
          uint16_t val = (pixel << 2) | low;

          if (out.width == 2672 && val < 512)
            val += 2; // No idea why this is needed

          pixel = val;
        }
      }
    }
  }
}

} // namespace rawspeed
