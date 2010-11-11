//@File: iolib.c
//
// Input/output routines used by BCU programs & others
//@

#include <stdio.h>
#include <stdlib.h>
#include <string.h>	// strstr()

#include "base/errlib.h"
#include "aotypes.h"

#include "iolib.h"

static int debug=0;


//@Function: ReadAsciiFile
//
// Reads an ASCII file containing BCU configuration values.
// Returns a pointer to a newly allocated buffer for the file (that the calling
// program must free() after use), and places in <len> (if valid) the length
// in bytes of the configuration file. <len> can be NULL.
//
// Returns NULL if an error is encountered. In this case, <len> is undefined.
//
// Files must have one 32bit hexadecimal number per line, as in this example:
//
// 0x03430594
// 0x2854FE94
// 0xA9498201
//
// Lines not starting with "0x" are ignored, and anything on the line after the number
// is ignored too, so it is possibile to arbitrarily comment the file.
//@

unsigned char *ReadASCIIFile( char *path, int *len)
{
        FILE *fp;
        char linebuffer[32], *pos;
        uint32 *buffer;
        int BUFFER_INCREMENT= 1024;     // "step" to increment the buffer with, in bytes
                                        // (must be a multiple of 4)
        int counter;
        uint32 value;

        if (debug)
                printf("Reading file %s\n", path);

        // If this is a FITS file, use the appropriate routine
	if ((pos = strstr(path, ".fits")) != NULL)
                {
                pos[5]=0;
                return ReadFitsFile( path, len);
                }

        // Open input file
        if ((fp = fopen( path, "r")) == NULL)
                {
                fprintf(stderr, "File %s not found\n", path);
                return NULL;
                }
       // Allocate a small buffer
        buffer = (uint32 *)malloc (BUFFER_INCREMENT);
        if (!buffer)
                return NULL;

        // Read one line at a time. <counter> is a dword counter
        counter=0;
        while (fgets( linebuffer, 31, fp))
                {
                // Skip non-value numbers
                if (strncmp( linebuffer, "0x", 2))
                        continue;

                value = strtoul(linebuffer, (char **)NULL, 16);

                buffer[counter] = value;
                counter++;

                // Realloc the buffer if needed
                if ((counter*4 % BUFFER_INCREMENT) ==0)
                     {
                     buffer = (uint32 *)realloc( buffer, counter*4 + BUFFER_INCREMENT);
                     if (!buffer)
                        return NULL;
                     }
                }

        fclose(fp);

        // Set the external parameter for length
        if (len)
                *len = counter*4;

        return (unsigned char *)buffer;
}

//@Function: ReadBinaryFile
//
// Reads a binary configuration file from disk
//@

unsigned char *ReadBinaryFile( char *path, int *len)
{
        FILE *fp;
        unsigned char *buffer;
	char *pos;
        int BUFFER_INCREMENT= 1024;     // "step" to increment the buffer with, in bytes
        int counter, num, offset;

        if (debug)
         printf("Reading file %s\n", path);

        // If this is a FITS file, use the appropriate routine
	   if ((pos = strstr(path, ".fits")) != NULL)
                {
                if (debug) printf("E' un file fits\n");
                //pos[5]=0;
                //printf("Terminato\n");
                return ReadFitsFile( path, len);
                }

        // Open input file
        if ((fp = fopen( path, "r")) == NULL)
                {
                fprintf(stderr, "File %s not found\n", path);
                return NULL;
                }

        if (debug) printf("file opened\n");

        // Allocate a small buffer
        buffer = (unsigned char *)malloc (BUFFER_INCREMENT);
        if (!buffer)
                return NULL;

        if (debug) printf("fist buffer OK\n");
       // Read one buffer each time
        counter=1;
        offset = 0;
        while ( (num = fread( buffer+offset, 1, BUFFER_INCREMENT, fp)) == BUFFER_INCREMENT)
                {
                counter++;
                buffer = (unsigned char *)realloc( buffer, BUFFER_INCREMENT*counter);
                offset = offset + BUFFER_INCREMENT;
                }

        fclose(fp);

        // Set the external parameter for length
        if (len)
                *len = offset+num;

        if (debug) printf("File read\n");
        return buffer;
}


//@ ReadFitsFile
//
// Simple function to read a single databuffer from a fits file. Reads the first HDU.
//@

unsigned char *ReadFitsFile( char *path, int *len)
{
    fitsfile *fptr;
    int fits_status =0; // MUST initialize status
    unsigned char *buffer = NULL;

    if (debug) printf("Opening -->%s<--\n", path);

    fits_open_file( &fptr, path, READONLY, &fits_status);
    fits_report_error(stdout, fits_status);

    if (fits_status == 0)
         {
         buffer = ReadFitsHDU( fptr, len, 0, NULL, NULL, NULL);
         fits_close_file(fptr, &fits_status);
         }

    return buffer;
}

//@ PeekFitsFile
//
// Returns some basic information about a fits file (datatype and dimensions)
// @
//@

int PeekFitsFile( char *path, int *datatype, int *ndims, long **dims)
{

    fitsfile *fptr;
    int fits_status =0; // MUST initialize status

    if ((!datatype) || (!ndims) || (!dims))
      return NULL_BUFFER_ERROR;

    if (debug) printf("Opening -->%s<--\n", path);

    fits_open_file( &fptr, path, READONLY, &fits_status);
    fits_report_error(stdout, fits_status);

    if (fits_status != 0) {
        fits_close_file(fptr,&fits_status);
        return FILE_ERROR;
    }
    else {

       fits_get_img_type( fptr, datatype, &fits_status);
       fits_report_error(stdout, fits_status);

       fits_get_img_dim( fptr, ndims, &fits_status);
       fits_report_error(stdout, fits_status);

       if (fits_status != 0) {
           fits_close_file(fptr,&fits_status);
           return FILE_ERROR;
       }

       *dims = (long *)malloc( *ndims * sizeof(long));
       fits_get_img_size( fptr, *ndims, *dims, &fits_status);

       if (fits_status != 0) {
           fits_close_file(fptr,&fits_status);
           return FILE_ERROR;
       }

       fits_close_file(fptr, &fits_status);
       return NO_ERROR;
       }

   return FILE_ERROR;
}



//@ReadFitsFileWithLUT
//
// Read a fits file and searches for an additional HDU containing the reordering LUT.
//@

unsigned char *ReadFitsFileWithLUT( char *path, int *len, int maxdim, int *ndims, long *dims, int **lut, int *lutlen)
{
    fitsfile *fptr;
    int fits_status =0; // MUST initialize status
    unsigned char *buffer = NULL;
    int datasize;

    if (debug) printf("Opening -->%s<--\n", path);

    fits_open_file( &fptr, path, READONLY, &fits_status);
    fits_report_error(stdout, fits_status);

    if (fits_status) {
       if (lut)
          *lut=NULL;
       return NULL;
    }

    buffer = ReadFitsHDU( fptr, len, maxdim, ndims, dims, &datasize);

    if (len)
       *len /= datasize;

    // See if you can get a LUT
    int hdunum;
    fits_get_num_hdus( fptr, &hdunum, &fits_status);

    if (hdunum<2)
      {
      printf("Warning: no LUT hdu found!\n");
      if (lut)
      	*lut = NULL;
      if (lutlen)
         *lutlen = 0;
      fits_close_file(fptr, &fits_status);
      return buffer;
      }

    // Read LUT
    int type;
    fits_movabs_hdu( fptr, 2, &type, &fits_status);
    maxdim=1;
    *lut = (int *)ReadFitsHDU( fptr, lutlen, maxdim, NULL, NULL, NULL);

    fits_close_file(fptr, &fits_status);

    return buffer;
}













//@ ReadFitsHDU
//
// Internal function to read one HDU from an openened FITS file
//@

unsigned char *ReadFitsHDU( fitsfile *fptr, int *len, int maxdim, int *naxis, long *axes, int *getdatasize)
{
    int fits_status =0; // MUST initialize status
    int size, datatype, datasize, i, bitpix;
    long *zeropos = NULL;
    unsigned char *buffer = NULL;
    int local_naxis=0;
    long *local_axes=NULL;

    if (naxis == NULL)
       naxis = &local_naxis;
    if (axes == NULL)
      {
      maxdim=3;
      local_axes=(long *)malloc(sizeof(long)*maxdim);
      axes = local_axes;
      }

    fits_get_img_type( fptr, &bitpix, &fits_status);
    fits_report_error(stdout, fits_status);

    fits_get_img_dim( fptr, naxis, &fits_status);
    fits_report_error(stdout, fits_status);
       
    if (fits_status != 0) {
        fits_close_file(fptr,&fits_status);
        if (local_axes)
           free(local_axes);
        return NULL;
    }

    switch(bitpix)
        {
        case BYTE_IMG:
        datatype = TBYTE;
        datasize= sizeof(char);
        break;

        case 16:
        datatype = TSHORT;
        datasize= sizeof(short);
        break;

        case 32:
        datatype = TINT;
        datasize= sizeof(int);
        break;

        case -32:
        datatype = TFLOAT;
        datasize= sizeof(float);
        break;

        case -64:
        datatype = TDOUBLE;
        datasize= sizeof(double);
        break;

        default:
        printf("Error: unknown bitpix %d\n", bitpix);
        fits_close_file(fptr,&fits_status);
        return NULL;
        break;
        }

    if (getdatasize)
	*getdatasize = datasize;

    if (debug) {
        printf("Bitpix = %d\n", bitpix);
        printf("N. axis = %d\n", *naxis);
        printf("Datasize = %d\n", datasize);
     }

    if (*naxis > maxdim) {
       printf("Error: unsupported naxis %d (maximum is %d)\n", *naxis, maxdim);
       if (local_axes)
          free(local_axes);
       return NULL;
    }
      
    zeropos = (long *)malloc( *naxis * sizeof(long));
    fits_get_img_size( fptr, *naxis, axes, &fits_status);

    size = 1;
    for (i=0; i<*naxis; i++)
        {
        if (debug)
                printf("Axis %d: %d\n", i+1, (int) axes[i]);
        zeropos[i]=1;
        size = size * axes[i];
        }

    if (debug)
            printf("total size is %d elements, %d bytes\n", size, size*datasize);

    buffer = (unsigned char*)malloc(size*datasize);
    fits_read_pix( fptr, datatype, zeropos, size, NULL, (void *)buffer, NULL, &fits_status);

    free(zeropos);

    if (len)
       *len = size * datasize;

    if (local_axes)
       free(local_axes);

    return buffer;
}


//@WriteFitsFile
//
// Writes a simple fits file with only one HDU.
// Type must be one of TFLOAT, TDOUBLE or TSHORT
// The length of the buffer is expressed as the number of axis
// in the image and their length. Each element counts for 4 bytes.
//@


int WriteFitsFile( char *path, unsigned char *buffer, int type, long *dims, int ndims) {
   return WriteFitsFileWithKeywords( path, buffer, type, dims, ndims, NULL, 0);
}

int WriteFitsFileWithKeywords( char *path, unsigned char *buffer, int type, long *dims, int ndims, fitskeyword *keywords, int nkeywords) {

   fitsfile *fptr;
   int status =0; // MUST initialize status
	long *fpixel;
	int n_elements, i;

   int imgtype;
   switch(type) {
      case TINT:
         imgtype = LONG_IMG;
         break;
      case TFLOAT:
         imgtype = FLOAT_IMG;
         break;
      case TDOUBLE:
         imgtype = DOUBLE_IMG;
         break;
      case TSHORT:
         imgtype = SHORT_IMG;
         break;
      default:
         return -1;
   }

	fits_create_file( &fptr, path, &status);
	fits_create_img( fptr, imgtype, ndims, dims, &status); 
   if (status != 0) {
       fits_report_error(stdout, status);
        fits_close_file(fptr,&status);
        return -1;
    }

   if (nkeywords>0) {
      for ( i=0; i<nkeywords; i++) {
         fits_write_key( fptr, keywords[i].datatype, keywords[i].keyname, keywords[i].value, keywords[i].comment, &status);
         if (status != 0) {
            fits_report_error(stdout, status);
            fits_close_file(fptr,&status);
            return -1;
         }
      }
   } 

	fpixel = (long *)malloc(ndims * sizeof(long));
	n_elements = 1;
	for (i=0; i<ndims; i++) {
		fpixel[i] = 1;
		n_elements *= dims[i];
		}
	
	fits_write_pix( fptr, type, fpixel, n_elements, buffer, &status);
   if (status != 0) {
       fits_report_error(stdout, status);
        fits_close_file(fptr,&status);
        return -1;
    }
	fits_close_file( fptr, &status);

	free(fpixel);

	return NO_ERROR;
}

//@ WriteFitsFileWithLUT
//
// Writes a data buffer into a FITS file along with a reordering LUT.
//@

int WriteFitsFileWithLUT( char *path, unsigned char *buffer, int type, long *dims, int ndims, int *lut, int lutlen)
{
   fitsfile *fptr;
   int status =0; // MUST initialize status
	long *fpixel;
	int n_elements, i;

	fits_create_file( &fptr, path, &status);
	fits_create_img( fptr, type, ndims, dims, &status); 

	fpixel = (long *)malloc(ndims * sizeof(long));
	n_elements = 1;
	for (i=0; i<ndims; i++) {
		fpixel[i] = 1;
		n_elements *= dims[i];
		}
	
	fits_write_pix( fptr, type, fpixel, n_elements, buffer, &status);

   // See if a LUT must be written along
   if (lut) {
      long dims[1] = { lutlen };
      fits_create_img( fptr, TINT, 1, dims, &status);
      int type;
      fits_movabs_hdu( fptr, 2, &type, &status);  // ??
      fits_write_pix( fptr, TINT, fpixel, lutlen, lut, &status);
   }

	fits_close_file( fptr, &status);

	free(fpixel);
    return 1;
}

int ReadFitsKeyword( char *path, char *keyword, int datatype, void *value) {

    fitsfile *fptr;
    int fits_status =0; // MUST initialize status
    char comment[128];

    if ((!datatype) || (!value))
      return NULL_BUFFER_ERROR;

    if (debug) printf("Opening -->%s<--\n", path);

    fits_open_file( &fptr, path, READONLY, &fits_status);
    fits_report_error(stdout, fits_status);

    if (fits_status != 0) {
        fits_close_file(fptr,&fits_status);
        return FILE_ERROR;
    }
    else {
        fits_read_key(fptr, datatype, keyword, value, comment, &fits_status);
        if (fits_status != 0) {
            fits_close_file(fptr,&fits_status);
            return KEY_NOTFOUND_ERROR;
        }
        

       fits_close_file(fptr,&fits_status);
       return NO_ERROR;
       }

   return FILE_ERROR;
}

//@Function: writeFits3D
//
// Wrapper for WriteFitsFile to write 3d buffers
//
// Also available are writeFits2D and writeFits1D
//@

int writeFits3D( char *filename, void *buf, int type, int dim1, int dim2, int dim3) {

      long dims[] = { dim1, dim2, dim3 };
      return WriteFitsFile( filename, (unsigned char *) buf, type, dims, 3);
}

int writeFits2D( char *filename, void *buf, int type, int dim1, int dim2) {

      long dims[] = { dim1, dim2 };
      return WriteFitsFile( filename, (unsigned char *) buf, type, dims, 2);
}

int writeFits1D( char *filename, void *buf, int type, int dim1) {

      long dims[] = { dim1 };
      return WriteFitsFile( filename, (unsigned char *) buf, type, dims, 1);
}



