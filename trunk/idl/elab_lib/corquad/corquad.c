/*

Updated correction. See notes in README2.  
CAK 08Dec11
JCL 07Mar26
   
This routine corrects PISCES images for the negative "shadows" on all four
quadrants due to bright sources. It actually assumes that any flux in a
pixel has a negative effect on all 4 corresponding pixels in the 4
quadrants at about a 0.002 level, plus a "shadow" at a much lower level.

The output executable is called 'corquad', works stand-alone (not in any
particular package like IRAF) and expects 1 FITS file as input like:
> corquad filename.fits
The files do not have to be corrected for ushort int problems, any pixel
with <-2000 ADU will have 2^16=65536 added to it. 

The output file will be named "q"//infilename, and will be BITPIX=-32,
so single precision floats. 

To use this program in IRAF, add the following lines to your loginuser.cl
file: 
 task $corquad = $your_path_to_the_corquad_dir/corquad
 task cquad = your_path_to_the_corquad_dir/cquad.cl
You can now use the cquad IRAF script to multiple
call the corquad task.
cl> cquad *.fit
or
cl> cquad @filelist

The shape of the shadow was determined from a large stack of dark
frames, showing shadows from hot pixels, but having very low photon
noise. The shadow was fitted with a 4th order polynomial, except for the
central few pixels, as these pixels have a much larger negative offset.

The routine works quite simple:
-split the 4 quadrants of image in 1D arrays, correcting for pix <-1000
-convolve the shadow kernel with the individual quadrants
-sum the 4 convolutions
-subtract this sum from each of the 4 quadrants


If "illegal" (non-printable) characters end up in your FITS headers, 
corquad won't be happy.  Fix this first
(with iraf HEDIT for instance) before using this program.
cl > hedit dark*.fit COMMENT snowy ver-
*/


#define	KERNSIZE	120 /* size of the shadow kernel */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "fitsio.h"

void	printerror( int status);
void	convolve(float *insub, float *outsub, float *kernel);


int	main(argc, argv)
int     argc;
char    *argv[];

{
	fitsfile *infptr,*outfptr;	/* pointer to the FITS file, defined in fitsio.h */
	int	i, j, k, l;		/* loop variables */
	int	status;			/* subroutines return values */
	int	anynull, nullval=0;	/* don't check for undef values in ima*/
	float	*inima, *pin;		/* full in image */
	float	*outima, *pout;		/* full out image */
	float	*insub, *pinsub;	/* one input quadrant */
	float	*outsub, *poutsub;	/* sum off convolved quadrants */
	float	kernel[KERNSIZE];	/* kernal of the shadow */
	float   a0=-3.343291E-4,a1=7.858745E-6,a2=-7.819456E-8, a3=3.687237E-10,a4=-6.67760E-13;
	char	infilenm[FLEN_FILENAME];	/* input filename */
	char	outfilenm[FLEN_FILENAME];	/* output filename */
	char    tmp_name[120];
	char    tmp_name2[120];
        char    *next = NULL;
	char    dummy[32];
	char    *home;
	int	bitpix   = FLOAT_IMG;	/* 32-bit single precision floats */
	FILE    *fp;

        kernel[0] = -0.0031;
        kernel[1] = -0.0021;
        kernel[2] = -0.0006;

	strcpy(infilenm,argv[1]);
	
	/*
	  Copy infilenm into an auxilary character string because
	  the call to strtok() will destroy the passed string
	*/
	strcpy( tmp_name, infilenm);

	/*
	  Find the file name by removed the absolute path directories
	*/
	next = strtok(tmp_name, "/");

	while ( next != NULL ) {

	  strcpy( outfilenm, next);
	  printf("'%s'\n", outfilenm);
	  next = strtok(NULL, "/");

	}

	/*
	  Copy infilenm into an auxilary character string because
	  the call to strtok() will destroy the passed string
	*/
	strcpy( tmp_name, outfilenm);

	/*
	  Find the file name by removed the absolute path directories
	*/
	next = strtok(tmp_name, ".");
        strcpy( outfilenm, next);
        next = strtok(NULL, ".");
        i = 1;

	while ( next != NULL ) {

          if ( ( i != 1) ) {
            strcat( outfilenm, ".");
            strcat( outfilenm, tmp_name2);
          }
	  strcpy( tmp_name2, next);
	  next = strtok(NULL, ".");
          i++;

	}

       	strcat(outfilenm,".cq.fits");
	
	/* create some storage space */
	inima  = (float *)malloc(sizeof(float)*1024*1024);
	outima = (float *)malloc(sizeof(float)*1024*1024);
	insub  = (float *)malloc(sizeof(float)*512*512);
	outsub = (float *)malloc(sizeof(float)*512*512);


	// read in the file that has the corquad parameters in it
	home=getenv("HOME");
	
	if((fp = fopen(strcat(home,"/.corquad"), "r")) == NULL)
	  printf("Help -- I can't find your .corquad init file!\nUsing defaults.\n");
	else
	  {
	    fscanf(fp, "%s %f\n", dummy, &a0);
	    fscanf(fp, "%s %f\n", dummy, &a1);
	    fscanf(fp, "%s %f\n", dummy, &a2);
	    fscanf(fp, "%s %f\n", dummy, &a3);
	    fscanf(fp, "%s %f\n", dummy, &a4);
	    fscanf(fp, "%s %f\n", dummy, &kernel[0]);
	    fscanf(fp, "%s %f\n", dummy, &kernel[1]);
	    fscanf(fp, "%s %f\n", dummy, &kernel[2]);
	    fclose(fp);
	  }

	status=0;
	/* open input file */
	if ( fits_open_file(&infptr, infilenm, READONLY, &status) ) 
		printerror( status );

	/* map the input image */
	if ( fits_read_img(infptr, TFLOAT, 1, 1024*1024, &nullval,
	                  inima, &anynull, &status) )
		printerror( status );

	/* create output file with header identical to input */
	remove(outfilenm);
	if (fits_create_template(&outfptr, outfilenm, infilenm, &status)) 
		printerror( status );

	/* convert to 4-bit float output*/
	if ( fits_update_key(outfptr, TINT, "BITPIX", &bitpix, NULL, &status))
		printerror( status );

	
	/**************************************************/
	/* CAK 11dec2008 correction*/
	for (i=3;i<KERNSIZE;i++) {
		kernel[i] =a0 + a1*i + a2*i*i + a3*i*i*i +a4*i*i*i*i;
	}
	/**************************************************/

	/* printf("Convolving quadrants\n"); */

	/* split in 4 quadrants */
	for (i=0;i<2;i++){
	   for (j=0;j<2;j++){
	      pin = inima+i*512*1024+j*512;
	      pinsub = insub;
	      for (k=0;k<512;k++){
		for (l=0;l<512;l++){
		   /* to fix large negative unsigned short problems */
		   *pin = (*pin>-2000) ? *pin : 65536 + *pin;
		   *pinsub++ = *pin++;
		}
	      	pin += 512;
	      }
	      /* convolve the quadrant and add to outsub */
	      convolve(insub,outsub,kernel);
	   }
	}
	      


	/* printf("Subtracting convolved quadrants\n"); */

	/* subtract the convolved summed quadrant from all 4 quadrants */
	pout = outima;
	for (i=0;i<2;i++){
	   for (k=0;k<512;k++){
	      for (j=0;j<2;j++){
		for (l=0;l<512;l++){
		    *pout++ = *inima++ - *outsub++;
		}
		outsub -= 512;
	      }
	      outsub += 512;
	   }
	   outsub -= 512*512;
	}
		
	
	/* printf("Write FITS file to disk\n"); */

	/* write the array to the FITS file */
	if ( fits_write_img(outfptr, TFLOAT, 1, 1024*1024, outima, &status) )
	        printerror( status );

	/*close the in & out files */
	if ( fits_close_file(infptr, &status) )
		printerror( status );
	if ( fits_close_file(outfptr, &status) )
		printerror( status );

	return 0;
}



/*--------------------------------------------------------------------------*/

void convolve(float *insub, float *outsub, float *kernel)
{
	int	j, k;
	
	/* first the first bit till KERNSIZE, where we have an if statement */
	for(j=0; j<KERNSIZE; j++){
	   for(k=0; k<KERNSIZE&&j-k>=0; k++) {
		*outsub += kernel[k]*insub[-k];
	   }
	   insub++;
	   outsub++;
	}
	/* now the rest */
	for(j=KERNSIZE; j<262144; j++){
	   for(k=0; k<KERNSIZE; k++) {
		  *outsub += kernel[k]*insub[-k];
	   }
	   insub++;
	   outsub++;
	}
}



/*--------------------------------------------------------------------------*/

void printerror( int status)
{
	/*****************************************************/
	/* Print out cfitsio error messages and exit program */
	/*****************************************************/
	            
	           
	if (status) {
		fits_report_error(stderr, status); /* print error report */
		exit( status );    /* terminate the program */
	}
	return;
}

