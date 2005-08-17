/* 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <HsFFI.h>

#include <stdio.h>
#include <sys/mman.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include <mhash.h>

/* Must be a multiple of the pagesize */
#define MD5_BLOCKSIZE 512 * 1048576 /* 512 MB */

char *media_md5sum(char *file, off_t size) {
  static char retval[33];
  unsigned char digest[20];
  char *buf;
  MHASH md;
  off_t procoffset = 0;
  size_t chunksize;
  off_t remaining;
  int i, fd;

  md = mhash_init(MHASH_MD5);
  if (md == MHASH_FAILED) {
    fprintf(stderr, "Couldn't initialize mhash system\n");
    return NULL;
  }
  
  fd = open(file, O_RDONLY);
  if (fd == -1)
    return NULL;
  
  while (size > procoffset) {
    remaining = size - procoffset;
    chunksize = remaining > MD5_BLOCKSIZE ? MD5_BLOCKSIZE : (size_t)remaining;
    
    buf = mmap((void *) 0, chunksize, PROT_READ, MAP_SHARED, fd, procoffset);
    if (buf == MAP_FAILED)
      return NULL;

    mhash(md, buf, chunksize);

    if (munmap(buf, chunksize) == -1)
      return NULL;
    
    procoffset += (off_t) chunksize;
  }
  close(fd);

  mhash_deinit(md, digest);
  
  for (i = 0; i < 16; i++) {
    sprintf(retval + (i * 2), "%.2x", digest[i]);
  }

  return retval;
}
