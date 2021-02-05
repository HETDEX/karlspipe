Chenxu's process

1 flats

  1.1 the scripts
  
    /work/05178/cxliu/wrangler/hdr3/flat/script/*
    /work/05178/cxliu/wrangler/hdr3/flat/rsetflat
      
  1.2 the list to run
      
     /work/05178/cxliu/wrangler/hdr3/flat/rsetflat0
      
  1.3 how to run, take one in rsetflat0 as an example
  
      rsetflat 409 016 20210122 7000
      
  This will submit two jobs, the first one will go immediately and the second one will go after the first one.
      Once both jobs are finished, do
      
      cd 20210122; rt3; cd .. 
      
   This will create four fits files for the flats of the four amps for cam409: pixelflat_cam409_LL.fits, pixelflat_cam409_LU.fits, pixelflat_cam409_RL.fits, pixelflat_cam409_RU.fits
      
  1.4 mask the bad pixels in the flats
      
      /work/05178/cxliu/wrangler/hdr3/flat/Pixel_flat_masks.ipynb
   This jupyter notebook will create the mask file with the format below.
       
      cat  /work/05178/cxliu/wrangler/hdr3/flat/pix_mask_cx.tab 
      amp x1 x2 y1 y2
      311RU 538 556 755 737

2 bias

  2.1 the execute folder
  
    /work/05178/cxliu/wrangler/hdr3/bias
 
  2.2 the scripts
  
     cp /work/00115/gebhardt/hdr/karlspipe/hdr2.1/bias/* .
     cp /work/00115/gebhardt/maverick/gettar/r1.202* .
 
 2.3 files to run: from Aug 2020 to Jan 2021
        
	r1.2*

 2.4 how to run (take the first one as an example)
  
     mkdir 202008bias
     /home/05178/cxliu/bin/rjob r1.202008 1 "04:00:00" # wrangler
     /home1/05178/cxliu/bin/rjob r1.202008 1 "04:00:00" # stampede2
     
 2.5 files expected
 
     /work/05178/cxliu/wrangler/hdr3/bias/202*/*.fits

--------------------------------------
Below is Karl's original readme file
--------------------------------------
# reduction

1) List of fits files: read all tarfiles from a given month and just
make a list of fits files.  rstep1 in dbase generally takes about an
hour, and we should use the queue.

scripts/lists: rstep1, rgetrep, badall2

These tarlists become the reference files for all reductions. If there
is data that is bad, it should be removed from the tarlists. For
example, all repeat fits fiels are removed with:
rgetrep ${mth}
and this will produce a new tarlist, based on the badall2 list.

2) List of science frames: get header information from the science
frames for a month. This requires untarring one frame from each field,
and then extracting the information one wants.

scripts/lists: run1, rtar, rgetsci1

It is run with:
rm -f scinew
run1 ${mth}tarlist > rt
chmod +x rt; rt
mv scinew ${mth}sci

3) Generation of all reductions scripts: For each science and twilight
fits file, generate the program calls.

scripts/list: rsetvred, rsetcal, listall

First, call the fplane server for every night, and results output in a
directory. To generate the fplane for every night, generate a file
called datelist, and execute:

python getfplane2.py

Then to generate the scripts for the reductions:
ls /work/00115/gebhardt/maverick/fplane/fp${mth}* | awk '{print "\""$1"\""}' > listall
rsetvred ${mth}
rsetcal ${mth}

These will generate files that look like:
rall202001  : the background modeling scripts
rt1.202001  : step 1 of reduction script
rt1b.202001 : step 2 of reduction script
rt1c.202001 : step 3 of reduction script
rt2.202001  : step 4 of reduction script
rt3.202001  : step 5 of reduction script
rta.202001  : call for vdrp/astrometry
runs202001  : base call for each amplifier sci reduction
runt202001  : base call for each amplifier twi reduction
r1.202001   : call to generate bias frames for the month

Due to swaps of IFUs to spectrographs, you will need to modify some of
the runs and runt scripts. This is traked in readme.hist, which also
contains the modificatoons to the runs scripts.

4) Get the monthly biases: find all bias frames and combine by
detector and controller. Smooth with a 1x7 box, find extreme charge
traps, and place in a library.

scripts/list: rpbias, rpbias1, rbfits, rbfits0, rimcmb, r1, rtrap, rtrap0
code: mkbiaslist.f, imcmb.f, imbox2.f, improcb.f, imgetctrap.f

Make a directory called ${mth}bias, and then it is run with the r1
command, and needs to be run in batch.

After completion, run rtrap0 to find the extreme traps.

mv ${mth}bias/masterbias*fits /data/00115/gebhardt/lib_calib/lib_mbias/${mth}/.

5) Get the background model: Find all science and twilight frames, and
run 3 different sets of models accoring to backgroune level. These are
called twi, bri, sci.

scripts/files: rpsci0kg, rpsci1kg, rptwi0kg, rgfits, rgfits0, rmkdir
code: improcn.f, imcmb.f, imgetgap.f

This is run with the rall${mth} command, and needs to be done in
batch. First run "rmkdir ${mth}" to make the 3 subdirectories.

6) Twilight calibration: setup the twilight calibration and then run
with a 6-step process

scripts/lists: rsetupc, and everything in twical
code: vred.f, getata.f, imcmbbs.f, getxtr.f, getwavo.f, vred2.f, imgetpix.f, plotcaliball.f, plotfibpos.f

It is run with:
restupc ${mth}
then: cd cal${mth}
rstep1  : initial reductions to get trace, fiber profile, wavelength, etc
rstep2  : combines all results for an amplifier into a master calibration
rstep3  : copies the masters to a library directory
rstep4  : uses the full field to get the amp-to-amp
rstep5  : combines amp-to-amp over a  month
rstep6  : copies to a library
These can only be run after the previous completes

Then one needs to inspect over the years as a quality check. This is done within:
twical/analysis/r*
with the scripts run1, run2, where png output is made for each.

For HDR2, there are many adjustments made, and these are all stored in analysis: readme.cal

7) Science Reductions: setup the science reductions for all sci fits
files, and then run with a 2-step process

scripts/lists: rsetups, everything in sci
code: vred, vrecon

It is run with:
rsetups ${mth}
cd ${mth}
rstep1
rstep2

the second step will untar the multifits, and you will need to specify where the tarfile live

8) vdrp/astrometry: the code we use to get the astrometric solutions
for every fiber. It also fits the relative normalization for each
frame based on the reconstructed image

scripts/list: the rta202001 scripts generated in 3)
code: all contained in vdrp (we need to generate a complete list), it requires pyhetdex, shuffle

It is run with:
runsh0 20200624 012 14.0492 54.38 1 > /dev/null 2>&1

For the ifuoffsets, look in ifuoff.

9) extract stars, fit FWHM and relative normalization: extract spectra of everything 8) found

scripts/list: rsetstar_w, everything in /work/00115/gebhardt/maverick/detect/cal_script, rsumspec
code: fitradecsp.f, biwt.f, mkrsprun.f

It is run with:
rsetstar_w 20190101 011

This will first extract stars at the input RA,DEC from a catalog. It
re-centers based on smallest chi^2. Then it fits the FWHM for the PSF
model. Then it re-runs extractions with the best FWHM and re-centers
again. The output is the fwhm and extracted spectra.

10) SED fits to stars: get the stars with 5 SDSS colors, and fits an SED

scripts/list: input list of stars with 5 colors, in the form of shuffle output
old code: SEDfitter.py

For HDR2 we use stellarSEDfits:
  https://github.com/grzeimann/stellarSEDfits

   qfit_gaia --filename sources.hdr2 --ebv 0.02 --outfolder output

This is run for each star in an IFU, and output SED written to a file with a unique name.
It is recommended to run under 5000 stars per task to most 
efficiently run in parallel.

11) g-band normalization for throughput: uses the g-band from SDSS (or
PS) and integrates over the stellar spectrum

scripts/list: rgettp0, some in cal_script
code: getsdssg.f

It is run with:
rgettp0 20190101 011

12) fits to SED for throughput: fits each star's SED to the star's
extracted spectrum, and then combines for a final response function.

scripts/list: rgettp0b, some in cal_script, rgetsumfib0
code: combsed.f, plotseda.f

It is run with:
rgettp0b 20190101 011 0.2 0.02

The two numbers are the limits for acceptance of the standard
deviation for an individual tp normalized to the average, and the
rms. For the ones that pass acceptance, they are biweighted together
to give the final response function.

9-12) is now contained in rallcal

13) fit polynomial to throughput curve: uses Robin's polynomial fit,
and fits 2 parameters, a normalization and slope. The fit is to either
the SED fit or the gband fit.

scripts/list: rfitpoly
code: fitpoly.f

It is run with:
rfitpoly 20190101 011

The output is directly read into HDR2.

14_a) making scripts for detections, calib, flux limits, full sky

in mkscripts: rall_scripts

14_b) generating flats from twilight

main is rmkflat0
run with: rmkflat 032 201801
need to change rmkflat to point to proper runt
uses imar_flt.f

14_c) generating residuals for each pixel over time

the main script is rt.use
uses imcmbext.f

14_d) generating chi^2 for each pixel over time

main script is rt
and mkchi0
used imcmbchi.f

14) Detections: There are two steps for detections, searching and refinement

scripts/list: mkdetrun, rgetifucen, rfitsp1, rfindcen, rcut, rdet0
code: fitradecsp.f

The first step is to generate the scripts. We first get the RA,DEC
centers of each IFU in each field. 
Step 1 is to run the first list of mkdetrun with the input list of fields; it will look likle:
rgetifucen 20190101 011

and then the second line of mkdetrun generates both the full detect
scripts and calib fiber generation scripts. These are called rdet0 and rcal0.

The entries in mkdetrun determine the search grid.

The output are the inputs into HDR2. These are:
*.mc   : output fit parameters and uncertainties
*.list : information on all fibers that contributed
*.spec : 1d spectrum

15) Stellar extractions for continuum sample: first generate list of
continuum sources, then center up, then extract

scripts/list: rfindmax, rgetbest, rgetmax, rmkcs, rmkcs2, rf1, rsp3fc
code: fitradecsp.f, matchradec2.f

The first step finds continuum sources with:
rgetmax 20190101 011

Current settings are if a fiber has a count of 50 per pixel in either
the blue or the red. The script also calls matchradec2 which combines
if within 5.5" (should probably make this smaller). This will generate
a file for each run called 20190101v011.cs.

rmkcs will then create the script (rcs0) which will peak up on the
continuum source. This needs to be done in batch.
sed -i s/"\/work\/00115\/gebhardt\/maverick\/scripts\/rsp\/rsp3fc"/rspcs/ rcs0

rmkcs2 will then create the final extraction script, called rext1.
rmkcs2 > rext1

Then run rext1 in batch to generate continuum spectra and the
information of the fibers that went into it.

awks.dat
need findmatch.f

16) Generate calibrated fibers: used for HDR2 ingestion only. These
calibrate all individual fibers, and generate multi-extension fits files.

scripts/list: rcal0
code: fitradecsp.f

Step 14) above generates the set of scripts call rcal0. Run this and it
will produce all calib and calibe fits files.

17) Generate H5 files:


All scripts are found at:

https://github.com/HETDEX/hetdex_api/tree/hdr2/h5tools

SHOT FILES
----------
For each DATE OBS, to create  single H5 shot file:

    python3 create_shot_hdf5.py -d DATE -o OBS -of DATEvOBS.h5 --tar
    python3 append_calfib.py -d DATE -o OBS -of DATEvOBS.h5
    python3 create_astrometry_hdf5.py -d DATE -o OBS -of DATEvOBS.h5 --append   
    python3 create_cal_hdf5.py -d DATE -o OBS -of DATEvOBS.h5 --append

Note that default directories used for HDR2 are in the hdr2 branch for
hetdex-api and can be found in hdr2/software/hetdex-api/h5tools for each
of the above programs.

You will need directories that contain astrometry output, throughput
output and the reduced multi*fits

The option --tar means the files are ingested from the
*mu.tar files generated from tarring the multi*fits. It is advised to
run all scripts in serial for each
shot. You can submit 240 jobs on 10 Nodes at a time on wrangler. It takes
between 10-30 min per shot (when the queue is running smoothly).

SURVEY AND MASTER FIBER FILE
----------------------------

	python3 create_survey_hdf5.py -of survey_hdr2.h5 -sl dex.hdr2.shotlist

You must provide the shot diretory (its default is obvious in the script).
This will combine every Shot table in the H5 produced above to make a
basic survey properties file. This can be done on an idev node as its quick.

      python3 create_fiber_index_hdf5.py -of fiber_index_hdr2.h5

Likewise, the above file collects the FiberIndex tables from every shot
file to make a master fiber file.

DETECTION H5 FILE
-----------------
Use chenxu's cat file (or just the mc.res files with a source list) to ingest
by month or from a defined directory. If doing by month (eg --month 201701 ) then
this means the .list and .spec files must be organized by month. Otherwise use
detect_path to specifc the directory where all files lie.

    python3 create_detect_hdf5.py -m 201901 -of detect_201901.h5

Run all months in a single job then combine with:
    
    python3 create_detect_hdf5.py --merge -of detect_hdr2.h5

To run continuum sources:

    python3 create_detect_hdf5.py -dp /data/00115/gebhardt/cs/spec /
-cs /data/00115/gebhardt/cs/rext1 -of continuum_sources.h5

Machine Learning Inputs
-----------------------

Break down the table hdr2/detects/detect_hdr2.tab into different shots so
you don't load massive tables in (helps to parallelize things). I call these
tables dets/det_SHOTID.tab
    
    python3 ../hetdex-api/hetdex_tools/get_spec2D.py --dets dets/det_SHOTID.tab /
                -dx 100 -dy 9 --h5file -s SHOTID

You can 240 of these on 10 Nodes at a time. Then combine the outputfiles
(format is im2D_SHOTID.h5) into a large merged H5 file:

    python3 get_spec2D.py --merge -dx 100 -dy 9 --h5file

Requires a directory populated with im2D*.h5 files. Will merge all of
these in the file merged_im2D.h5

18) Elixer:

Generates individual detection reports as PDFs and PNGs. Releated utilities
gather the reports into sqlite databases for easy access. ELiXer consumes
HDF5 files generated in previous steps.

location: https://github.com/HETDEX/elixer.git
Can also run directly from /work/03261/polonius/hetdex/science/sciscripts/elixer/elixer/selixer.py
or import and use bash wraper from /work/03261/polonius/usr/bin/selixer

See elixer/docs for more detailed documentation

Basic steps:
Can run from /data (wranger: recommended) or /scratch (stampeded) or /work

18.1) Save list of detectids to file (single column); no imposed limit,
      but recommend, depending on cluster resources, to keep < 100,000
      at a time (this is the <detection list file> referenced below)

18.2) Run the slurm version of ELiXer: (bash wrapper shown, with common
      switches shown; see --help or document for more detail)
      The slurm version automatically sets the request time and queues
      the request with TACC.

selixer --hdr 2 --dets <detection list file> --error 3.0 --name <outdir>
 --png --neighborhood 10.0 --mini --tasks 0 --nodes 4  --recover --blind
 --ooops --email <your email>

18.3) After the run is complete, merge the dispatched HDF5 files:

selixer --merge

18.4) You may wish to check that the output reports match the expected count,
      so, from the directory with all the dispatch_* folders, run:

find . -name "*[0-9].pdf" | wc

      You may also wish to check for .png, nei.png, and mini.png in addition
      to .pdf if those options were provided on the original elixer call.

      If the counts do not match, you may wish to re-run ELiXer to pickup
      any reports that failed. If re-run, ELiXer will attempt to build only
      the reports that do not already have a .pdf file. If you wish to
      also re-build those without imaging (usually due to a timeout or
      network problem) or with failed .png versions, run the following
      command 1st to clear out those associated PDFs so the detection
      will be picked up again.

elixer --prep_recover

    You may then re-run elixer either by re-issuing the exact command
    as in setp 18.2 (from the same location as the original call) or
    you may execute the following from the directory with the dispatch_*
    folders:

sbatch elixer.slurm

    Repeat 18.4 as needed to fill in any missing reports. Depending on
    the total number of detections to run, this may sometimes take 1 or
    2 follow up runs.

18.5) All the report files are in the various dispatch_* directories. You
      may now wish to combine them all into sqlite databases (recommended)
      for easy access (and reduced file handles).

      It is most efficient to open an idev session and run this operation
      from the node's /tmp folder (roughly 10x faster than running on
      /data or /scratch and some 25x faster than running on /work)

      You will be opening an idev session, copying all image files from
      their location(s) under the dispatch_* folders then inserting them
      into sqlite databse files and copying them back to /data, /scratch,
      or /work.

      Open an idev session: (the time shown here is 4 hours, but you may
      well need much more time, depending on the number of files).

      Insertion time (on /tmp) is 0.04-0.05s per file (almost regardless
      of size), plus the copy overhead (which is somewhat load dependent).
      Three hours should normally be enough for 100,000 detections (including
      the copy overhead). *Note: by default, the create and updates run
      concurrently (in the background) for each report type and each
      detectid prefix (every 100,000 detectids). If you edit the bash
      script (described below) to run serially, this will take roughly
      3x longer, but will reduce stress on the file system if that is
      a concern (*note: running in /tmp should NOT be a problem).

idev -A Hobby-Eberly-Telesco -t 03:00:00

    Inside the idev session, cd to /tmp and create a directory to work under
    (i.e. mkdir elixer) and cd to that directory and make directory to
    hold all the report images with THIS name (mkdir all_pngs)


    From the ELiXer source directory, copy the following files to your
    current directory:
    make_report_db.py and
    image_db_create.sh or image_db_update.sh *

    *(IF you are creating the sqlite datbases for the first time, you
    want image_db_create.sh, if you are appending to existsing databses,
    you want image_db_update.sh)

    Next copy all the image reports from the elixer run(s) into the all_pngs
    folder. This step can take a long time.
    (e.g. cp /data/<xxx>/<xxx>/<path to elixer run>/dispatch_*/*/*.png all_pngs)

    If you are updating existing sqlite databases either copy the .db files
    to be updated to your current directory (recommended) or edit the
    local copy of image_db_update.sh to set the db_dir path at the top
    of the file. If you copied the files locally, you will have to copy
    them back after this is complete. If you did not copy the files, they
    will be updated in their current location.

    If you are creating sqlite databases, you should edit the image_db_create.sh
    file and set the cp2dir at the top (the location the files will be copied
    to after creation).

    *** WARNING *** If you do not wish the creation/updates to run
    concurrently, be sure to remove the '&' from the end of the lines
    inside the for loop.

    Execute the script:

    source image_db_create.sh   or  source image_db_update.sh

    After complete, copy the databse files to their final location (if necessary).

    You may cd back to /tmp and rmdir the directory you created for this work,
    and exit from the idev session.

    *note: a list of final directories to serach for the sqlite databses
     is set at the top of hetdex_api/sqlite_utils.py

18.6) merging elixer_cat.h5 files

     Each elixer run produces 1 or more elixer_cat.h5 files. These are
     merged together when you execute elixer --merge (step 18.3)
     to produce a file named elixer_merge_cat.h5

     If you have multiple elixer_merge_cat.h5 files (e.g. after running
     elixer with different detection lists), you can merge them together
     one pair at a time with:

     elixer --merge_unique <file1>,<file2>,<file3>

     where file1 is the outout merged h5 file and file2 and file3 are
     the two input h5 files to combine. Repeat as necessary to arrive
     at a single h5 file, as needed.

*** Notice: referenced above are calls to elixer or selixer. This is
    correct if you are using the bash wrappers under the directory
    listed at the top of section 18. If not you will need to invoke via
    python (note TACC currently has 'python' aliased to 'python2' so you
    must use 'python3') as:

    python3 <path to elixer.py or selixer.py>

19) 2d spectra for all detections: version in the API as well as old
school.

20) Full-field sky subtractions: for each field and each exposure,
search for all multifits and then generate a full-field sky
model. Subtract each amplifier, with tweaks to each model, and produce
one frame. Then add this frame into the calib/calibe multifits.

script/lists: rfft, raddsky
code: vred3.f, imaddsky.f

It is run with:
rfft 20200203 023 exp03 202002
and it will write this to output, with three files: 
d20200203s023exp03amp.dat : amplifier information
d20200203s023exp03ds9.reg : ds9 regions file
d20200203s023exp03sub.fits : all fibers in a shot, sky subtracted in counts

The amplifier information looks like:
Spc_slt_iid_am Factor N_c    Avg  Scale Woff
008_093_054_LL  1.000   1    0.02   16.04   -0.12
008_093_054_LU  1.001   2   -0.06   16.13    0.12
008_093_054_RL  0.988   0    0.02   14.88   -0.12
008_093_054_RU  0.988   1   -0.10   15.15    0.38
012_106_033_LL  1.034   6   -0.06   16.18    0.12
These are:
1) spectrograph pairing
2) normalization factor to multiply full-frame sky to the local one
3) number of fibers removed as identified as continuum sources
4) average background of region with no continuum
5) rms of fiber with no continuum 
6) wavelength offset applied from the full sky
7) wave1
8) Nlow

