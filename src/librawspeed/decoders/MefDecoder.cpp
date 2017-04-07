/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2009-2014 Klaus Post
    Copyright (C) 2014 Pedro Côrte-Real

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

#include "decoders/MefDecoder.h"
#include "common/Common.h"                          // for uint32
#include "common/Point.h"                           // for iPoint2D
#include "decompressors/UncompressedDecompressor.h" // for UncompressedDeco...
#include "io/Buffer.h"                              // for Buffer
#include "io/Endianness.h"                          // for Endiannes
#include "tiff/TiffEntry.h"                         // for TiffEntry
#include "tiff/TiffIFD.h"                           // for TiffIFD, TiffRoo...
#include "tiff/TiffTag.h"                           // for TiffTag::STRIPOF...
#include <memory>                                   // for unique_ptr

namespace rawspeed {

RawImage MefDecoder::decodeRawInternal() {
  auto raw = mRootIFD->getIFDWithTag(STRIPOFFSETS, 1);
  uint32 width = raw->getEntry(IMAGEWIDTH)->getU32();
  uint32 height = raw->getEntry(IMAGELENGTH)->getU32();
  uint32 off = raw->getEntry(STRIPOFFSETS)->getU32();
  uint32 c2 = raw->getEntry(STRIPBYTECOUNTS)->getU32();

  if (c2 > mFile->getSize() - off) {
    mRaw->setError("Warning: byte count larger than file size, file probably truncated.");
  }

  mRaw->dim = iPoint2D(width, height);
  mRaw->createData();

  UncompressedDecompressor u(*mFile, off, mRaw);

  u.decode12BitRaw<big>(width, height);

  return mRaw;
}

void MefDecoder::decodeMetaDataInternal(const CameraMetaData* meta) {
  setMetaData(meta, "", 0);
}

} // namespace rawspeed
