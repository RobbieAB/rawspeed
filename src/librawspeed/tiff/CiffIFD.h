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

#pragma once

#include "common/Common.h"  // for uint32
#include "tiff/CiffEntry.h" // IWYU pragma: keep
#include "tiff/CiffTag.h"   // for CiffTag
#include <map>              // for map
#include <memory>           // for unique_ptr
#include <string>           // for string
#include <vector>           // for vector

namespace rawspeed {

class Buffer;

class CiffIFD final {
  CiffIFD* parent;

public:
  CiffIFD(CiffIFD* parent, Buffer* f, uint32 start, uint32 end,
          uint32 depth = 0);

  std::vector<std::unique_ptr<CiffIFD>> mSubIFD;
  std::map<CiffTag, std::unique_ptr<CiffEntry>> mEntry;

  void checkOverflow();
  void add(std::unique_ptr<CiffIFD> subIFD);
  void add(std::unique_ptr<CiffEntry> entry);

  std::vector<CiffIFD*> getIFDsWithTag(CiffTag tag);
  CiffEntry* getEntry(CiffTag tag);
  bool __attribute__((pure)) hasEntry(CiffTag tag);
  bool __attribute__((pure)) hasEntryRecursive(CiffTag tag);
  CiffEntry* getEntryRecursive(CiffTag tag);
  CiffEntry* getEntryRecursiveWhere(CiffTag tag, uint32 isValue);
  CiffEntry *getEntryRecursiveWhere(CiffTag tag, const std::string &isValue);
  std::vector<CiffIFD *> getIFDsWithTagWhere(CiffTag tag, const std::string &isValue);
  std::vector<CiffIFD*> getIFDsWithTagWhere(CiffTag tag, uint32 isValue);
  Buffer* getBuffer() { return mFile; }

protected:
  Buffer* mFile;
  uint32 depth;
};

} // namespace rawspeed
