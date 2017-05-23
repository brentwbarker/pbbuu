#!/bin/bash
#
# Compiles, runs, and analyzes results for various input parameters of buu code.
# Saves output to specified data directory.
#
# NOTE: does not currently analyze results. Use other scripts for this. All
# references to ansti.for have been commented out.
#
# Brent Barker, me@brentwbarker.net, 2006-07-08
# Last Modified 2016-06-17

########## INPUT ##########

## for-loops are run with string variables. If an underscore
# precedes a variable, then a for-loop is run for that variable.
# For example, 
#    _E="0.4 0.8"
# runs the code twice, once with E=0.4 and again for E=0.8

# systems available:
# 40ar58fe, 40ar58ni, 40ar70zn, 197au197au,40ca40ca,40ar63cu,40ar107ag,
# 40ar197au,96ru96ru,96ru96zr,96zr96zr,
# 84kr93nb, 86kr93nb, 40ar45sc,124sn124sn
_system="40ar58fe"

# cross-sections available:
# LCSM, rostockCS, fuchsCS, zhangCS, free
_crossSection="LCSM"

# impact parameter:
_impactParameter="2.52 2.52" #40+58,bred=0.3
#_impactParameter="2.52 0.0 0.838 4.19 5.87 7.55" #40+58,bred=0.3,0,0.1,0.5,0.7,0.9
#_impactParameter="0.0 0.90 1.8 2.7 3.6 4.5 5.4 6.3 7.2 8.1 9.0"


# number of statistics (300 is about 1.5 hours for one run of Au+Au)
NQU=8000

## EOS attributes
# choices: soft, stiff
ni=soft
## momentum dependence
# dep,indep
mo=dep

# produce correlation file
ncor=true

_useCompositeProduction="true"
_FIMCSnums="0.6"    # factor for LCSM (if not using LCSM, this is ignored)
_E="0.04" # system energy in GeV/nucleon

# random number seed (set to -1 to generate during runtime). Otherwise, must be
# odd and positive between 0 and 2^31-1
randseed=-1;

# working directory (absolute or relative from pwd)
wpath=~/proj/pbuu/working

# data directory (absolute or relative to wpath above)
datadir=~/proj/pbuu/data

# directory of pure code to copy from (absolute or relative to pwd)
puredir="/home/bbarker/proj/pbuu/pawel-2015"

# arbitrary string appended to results line
comment="heilborn-verde"
#comment="stopping-varxz"

######### COMMAND-LINE INPUT ##########

syntax() {

        echo "Usage: $0 [-b impact_parameter] [-e energy(s)] [-r events] [-s system(s)] [-w working_dir] [-i fimcs(s)] [-c crossSection(s)] -o [corrfile t/f] -p [comp-pro t/f] -m [mo dep/indep] -n [eos soft/stiff]"
        exit 64
}

let "args = $#"

if [[ `expr $args : [0,2,4,6,8,10,12,14,16,18,20,22,24]` -eq "0" ]] #if num of args is not correct
then
        syntax
fi

while [ "$args" -gt "0" ] # we haven't processed all args
do
  case $1 in
    -a)
      shift
      randseed=$1
      ;;
    -b)
      shift
      _impactParameter=$1
      ;;
    -c)
      shift
      _crossSection=$1
      ;;
    -e)
      shift
      _E=$1
      ;;
    -i)
      shift
      _FIMCSnums=$1
      ;;
    -n)
      shift
      ni=$1
      ;;
    -o)
      shift
      ncor=$1
      ;;
    -p)
      shift
      _useCompositeProduction=$1
      ;;
    -m)
      shift
      mo=$1
      ;;
    -w)
      shift
      wpath=$1
      ;;
    -r)
      shift
      NQU=$1
      ;;
    -s)
      shift
      _system=$1
      ;;
    -t)
      shift
      comment=$1
      ;;
    *)
      syntax
      ;;
  esac
shift
args=$#
done


########## RUN ##########

# create directories if they don't already exist
mkdir -p ${wpath}
mkdir -p ${datadir}

# check to make sure wpath != puredir, since we remove all files from wpath
#touch ${wpath}/wpath.lock
#[ -f ${puredir}/wpath.lock ] && (echo "wpath.eq.datadir :(" && exit 64)

# clear working directory to prevent previous runs from touching this one.
rm -rf ${wpath}/*

# copy code to working directory
cp -ar ${puredir}/* ${wpath}/

cd ${wpath}

# The run code requires a 3-character prefix. It then
# appends a 4-character pseudorandom string to the end
# of those 3 to create the filename (minus the .DAT, etc)
# I don't know on what parameters exactly the string is
# based, so in case it gets repeated within a run, I assign
# a different 3-character prefix to each iteration. To
# increment, I start at 10 and add 1 each time. That gives a
# maximum 90 iterations per run before it becomes 4 characters
# long. This pseudorandom number may be based on all input
# parameters. If it is, then you can safely change ${prefix}
# below to a constant 3 character string.
num=10

# test to ensure that "systems" file is present
[ -f systems ] || { echo systems file missing && exit 64; }

# set EOS stiffness
case $ni in
  stiff) ninum=1;;
  soft) ninum=0;;
  *)  echo Invalid EOS specification: ${ni}.
      exit 64
      ;;
esac

perl -i -pe "s/NI=.*\)/NI=$ninum\)/" NUCLS

# set momentum dependence
case $mo in
  indep) monum=0;;
  dep) monum=1;;
  *)  echo Invalid mo-dep: ${mo}.
      exit 64
      ;;
esac

perl -i -pe "s/MO=.*\)/MO=$monum\)/" NUCLS

# set correlation file production
case $ncor in
  false) ncorn=0;;
  true) ncorn=1;;
  *) echo Invalid corr-setting: ${ncor}.
     exit 64
     ;;
esac

perl -i -pe "s/NCOR=.*\)/NCOR=${ncorn}\)/" NUCLS

if [[ $randseed = "-1" ]]
then
 autogenerate_randseed=1
else
 autogenerate_randseed=0
fi

########## START for-LOOPS ##########

for system in ${_system}
do

# test that system exists in file
[ `awk "(\\$1 ~ /$system/)" < systems | wc -l` -gt 0 ] ||
    { echo Invalid system && exit 64; }

# get system line
sysline=`awk "(\\$1 ~ /$system/)" < systems | head -n1`

# extract system info
projectileA=`echo $sysline | awk '{print $2}'`
projectileZ=`echo $sysline | awk '{print $3}'`
targetA=`echo $sysline | awk '{print $4}'`
targetZ=`echo $sysline | awk '{print $5}'`

# set system info
perl -i -pe "s/IA1=.*,/IA1=${projectileA},/" NUCLS #ansti.for
perl -i -pe "s/IZ1=.*,/IZ1=${projectileZ},/" NUCLS #ansti.for
perl -i -pe "s/IA2=.*\)/IA2=${targetA}\)/" NUCLS #ansti.for
perl -i -pe "s/IZ2=.*\)/IZ2=${targetZ}\)/" NUCLS #ansti.for
perl -i -pe "s/NSTEP=.*,NPIC/NSTEP=${nstep},NPIC/" TSTEP

# set number of statistics
perl -i -pe "s/NQU=.*\)/NQU=${NQU}\)/" NQUA1
#perl -i -pe "s/NFI\*.*\//NFI\*${NQU}\//" ansti.for

for impactParameter in ${_impactParameter}
do

# set impact parameter
perl -i -pe "s/bim=.* +\!/bim=${impactParameter}     \!/" buu258.FOR

for crossSection in ${_crossSection}
do
# set cross section
LCSM=false
rostockCS=false
fuchsCS=false
zhangCS=false
_FIMCS="999"

case $crossSection in 
LCSM)
    LCSM=true
    _FIMCS=${_FIMCSnums}
    ;;
rostockCS)
    rostockCS=true;;
fuchsCS)
    fuchsCS=true;;
zhangCS)
    zhangCS=true;;
free)
    ;;
*)
    echo Invalid CS entry: ${crossSection}.
    exit 64
    ;;
esac

perl -i -pe "s/LCSM=\..*\./LCSM=\.${LCSM}\./" NUCLS
perl -i -pe "s/rostockCS=\..*\./rostockCS=\.${rostockCS}\./" NUCLS
perl -i -pe "s/fuchsCS=\..*\./fuchsCS=\.${fuchsCS}\./" NUCLS
perl -i -pe "s/zhangCS=\..*\./zhangCS=\.${zhangCS}\./" NUCLS

for compPro in ${_useCompositeProduction}
do
    # set composite production
    case $compPro in
    true)
	noCompPro=false;;
    false)
	noCompPro=true;;
    *)
	echo Invalid compPro: ${compPro}.
	exit 64;;
    esac

    perl -i -pe "s/NODEP=\..*\./NODEP=\.${noCompPro}\./" NUCLS

    for FIMCS in ${_FIMCS}
    do
	#set FIMCS
	perl -i -pe "s/FIMCS=.*\)/FIMCS=$FIMCS\)/" NUCLS
	
	for E in ${_E}
	do
	    let "num=num+1"
	    let "realNum=num-10"
	    
	    echo Starting run $realNum - energy $E - fimcs $FIMCS
	    
	    # see discussion above
	    prefix=x${num}

	    #  if randseed=-1 above, then generate random random seed from
	    #+ /dev/urandom and set to odd number between 0 and 2^31-1
	    #+ (constraint of buu code)
	    if [[ $autogenerate_randseed = "1" ]]
	    then
		#  get 4 pseudorandom bytes, convert to octal and parse to get
		#+ number
		randseed=`head -c4 /dev/urandom | od -t u4 | awk '{print $2}'`;
		let "randseed>>=2"; # divide by 4 (get rid of least bit)
		let "randseed<<=1"; # multiply by 2 (now we have even number
				    # -le 2^31)
		let "randseed-=1";  # now we have odd number
	    fi

	    perl -i -pe "s/ISEED=[0-9]+/ISEED=${randseed}/" buu258.FOR
	    perl -i -pe "s/ISEED=[0-9]+/ISEED=${randseed}/" thomas66b.for
#	    perl -i -pe "s/ISEED\/.*\//ISEED\/${randseed}\//" ansti.for
		
	    # set E
	    perl -i -pe "s/TLAB=.*\)/TLAB=$E\)/" NUCLS #ansti.for
	    
	    # set filename prefix (must be 3 characters)
	    perl -i -pe "s/FNAME='\w+'/FNAME='${prefix}'/" NUCLS
	    
	    # set timestep and longitudinal Lorentz shift

	    # get system line
	    sysline=`awk "(\\$1 ~ /$system/) && (\\$6 ~ /$E/)" < systems`

	    # extract system info
	    nstep=`echo $sysline | awk '{print $7}'`
	    DT=`echo $sysline | awk '{print $8}'`
	    DLL=`echo $sysline | awk '{print $9}'`
	    
	    perl -i -pe "s/NSTEP=.*,NPIC/NSTEP=${nstep},NPIC/" TSTEP
	    perl -i -pe "s/DT=.*\)/DT=${DT}\)/" TSTEP
	    perl -i -pe "s/DLL=.*\)/DLL=${DLL}\)/" SIZE2
	    
	    # remove old object files
#	    make

	    echo Compiling prep
#	    pgf77 thomas5.for solpak.for -o thomas
	    ./make_thomas.sh || { echo make thomas failed; exit 64; }
#	    make

	    echo Running prep
	    ./thomas > prep-in-progress.stdout || { echo run thomas failed && exit 64; }
#	    rm log/prep-in-progress.stdout   # do not need
	    
	    echo Compiling run
#	    pgf77 mov131.for cspak2.for hig2com.for myfi.for rang.for \
#	          buu257.for coll16.for loc16.for -o buu
	    ./make_buu.sh || { echo make buu failed && exit 64; }
#	    make
	    
	    echo Running buu
	    ./buu inputsample.nml > run-in-progress.stdout || { echo run buu failed && exit 64; }
	    
	    # find file that "run" created
	    testFile=`ls --sort=time |     # list dir, sorted by time
		grep ${prefix}....\.DAT |  # find only correctly prefixed files
		sed 2,/^$/d |              # delete all but first entry (most recent)
		sed "s/${prefix}\(.*\)\.DAT/${prefix}\1/"` # take .DAT out
		
	    # set filename in "test"
#	    perl -i -pe "s/FNAME\/'.*'/FNAME\/'${testFile}'/" ansti.for

	    # set to 1 filename
#	    perl -i -pe "s/NFI=.*\)/NFI=1\)/" ansti.for
	    
	    # changing name of run stdout file
	    mv run-in-progress.stdout ${datadir}/${testFile}.run
	    mv prep-in-progress.stdout ${datadir}/${testFile}.prep
	    mv ${prefix}.PRF ${datadir}/${testFile}.PRF
	    mv ${prefix}.SHP ${datadir}/${testFile}.SHP
	    mv ${prefix}.TOP ${datadir}/${testFile}.TOP
	    
#	    echo Compiling test
#	    pgf77 test.for myfi.for -o ansti
#	    make ansti
	    
#	    echo Running test
#	    ./ansti > ${datadir}/${testFile}.anstiout
	    # appends log file with parameters and filename
	    echo $projectileA $projectileZ $targetA $targetZ $mo $ni $compPro $crossSection $E $impactParameter $FIMCS $nstep $DT $DLL `date +%F` ${NQU} ${randseed} ${testFile} $comment
	    echo $projectileA $projectileZ $targetA $targetZ $mo $ni $compPro $crossSection $E $impactParameter $FIMCS $nstep $DT $DLL `date +%F` ${NQU} ${randseed} ${testFile} $comment >> ${datadir}/results
	    
#	    echo test Complete

# clean up folder. We do not need these files.
# NOTE: the folder is now routinely overwritten, so no need to remove
#  individual files
#	    rm *.ANS
#	    rm *.TOP
#	    rm *.SHP
	    
	    # move data files to data folder
	    mv ${testFile}* ${datadir}
	    
            # compress all files and move to data folder (to inspect source files later, if needed)
            tar -czf ${testFile}-src.tar.gz *
            mv ${testFile}-src.tar.gz ${datadir}

#	    make
	done
    done
done
done
done
done
