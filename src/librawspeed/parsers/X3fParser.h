/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2013 Klaus Post

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

#pragma once

#include "common/Common.h"     // for uint32
#include "parsers/RawParser.h" // for RawParser
#include <map>                 // for map
#include <string>              // for string

namespace rawspeed {

class Buffer;

class ByteStream;

class RawDecoder;

class X3fDecoder;

class X3fDirectory
{
public:
  X3fDirectory() : id(std::string()) {}
  explicit X3fDirectory(ByteStream* bytes);
  uint32 offset{0};
  uint32 length{0};
  std::string id;
  std::string sectionID;
};

class X3fImage
{
public:
  X3fImage();
  X3fImage(ByteStream *bytes, uint32 offset, uint32 length);
  /*  1 = RAW X3 (SD1)
  2 = thumbnail or maybe just RGB
  3 = RAW X3 */
  uint32 type;
  /*  3 = 3x8 bit pixmap
  6 = 3x10 bit huffman with map table
  11 = 3x8 bit huffman
  18 = JPEG */
  uint32 format;
  uint32 width;
  uint32 height;
  // Pitch in bytes, 0 if Huffman encoded
  uint32 pitchB;
  uint32 dataOffset;
  uint32 dataSize;
};

class X3fPropertyCollection
{
public:
  void addProperties(ByteStream *bytes, uint32 offset, uint32 length);
  std::string getString( ByteStream *bytes );
  std::map<std::string, std::string> props;
};

class X3fParser final : public RawParser {
public:
  explicit X3fParser(Buffer* file);
  ~X3fParser();
  RawDecoder* getDecoder();

protected:
  void readDirectory();
  std::string getId();
  void freeObjects();
  ByteStream *bytes;
  X3fDecoder *decoder;
};

} // namespace rawspeed
