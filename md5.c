#define _FILE_OFFSET_BITS 64

#include <stdio.h>
#include <sys/mman.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include <beecrypt/beecrypt.h>
#include <beecrypt/md5.h>

#define MD5_BLOCKSIZE 512 * 1048576 /* 512 MB */

/* FIXME: error checking! */

char *md5sum(char *file, off_t size) {
  static char *retval[33];
  byte digest[20];
  byte *buf;
  md5Param md;
  off_t procoffset = 0;
  size_t chunksize;
  off_t remaining;
  int i, fd;

  md5Reset(&md);
  fd = open(file, O_RDONLY);
  
  while (size > procoffset) {
    remaining = size - procoffset;
    chunksize = remaining > MD5_BLOCKSIZE ? MD5_BLOCKSIZE : (size_t)remaining;
    
    buf = mmap((void *) 0, chunksize, PROT_READ, MAP_SHARED, fd, procoffset);
    md5Update(&md, buf, chunksize);
    munmap(buf, chunksize);
    procoffset += (off_t) chunksize;
  }
  close(fd);

  md5Digest(&md, digest);
  
  for (i = 0; i < 16; i++) {
    sprintf(retval + (i * 2), "%hhx", digest[i]);
  }

  return retval;
}

void main(int argc, char *argv[]) {
  char *filename = argv[1];
  struct stat statbuf;
  char *md5;

  stat(filename, &statbuf);
  md5 = md5sum(filename, statbuf.st_size);
  printf("md5sum: %s\n", md5);
}


  
