/*
    RawSpeed - RAW file decoder.

    Copyright (C) 2017 Roman Lebedev

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

#include <algorithm> // for min
#include <cassert>   // for assert
#include <numeric>   // for accumulate
#include <vector>    // for vector

namespace rawspeed {

inline std::vector<unsigned> sliceUp(unsigned bucketsNum, unsigned pieces) {
  std::vector<unsigned> buckets;

  if (!bucketsNum || !pieces)
    return buckets;

  buckets.resize(std::min(bucketsNum, pieces), 0U);

  // split all the pieces between all the threads 'evenly'
  unsigned piecesLeft = pieces;
  while (piecesLeft > 0U) {
    for (auto& bucket : buckets) {
      --piecesLeft;
      ++bucket;
      if (0U == piecesLeft)
        break;
    }
  }
  assert(piecesLeft == 0U);
  assert(std::accumulate(buckets.begin(), buckets.end(), 0UL) == pieces);

  return buckets;
}

} // namespace rawspeed
