#!/usr/bin/python
import sys, getopt
import fileinput


# # # # # # # # # # # # # # # # # # # # # # #  
# # # # #     Read The Input Arguments      #  
# # # # # # # # # # # # # # # # # # # # # # #  
args = sys.argv[1:]
flags = 'hi:n:m:'


if(len(args) < 3*2): # (*2 because of the flag for per input)
    print "Error! Need at least three input values:"
    print ""
    print 'Example:'
    print '   ./updateInputSIF.py -i inputfile.sif -n nextTemperature.nk -m material'
    print "Program reads the data energy range from 'nextTemperature.nk',"
    print "updates Energy_Range of 'inputfile.sif' accoardingly and "
    print "updates the material:"
    print "   &material"
    print '     Material = "nextTemperature" '
    print "/"
    sys.exit()

try:
    flags, args = getopt.getopt(args, flags)
except getopt.GetoptError:  
    print 'Error, mismatch between given input arguments and the input options!'          
    sys.exit(2)


# Loop Through Input (Flags,Arguments)
for flag, arg in flags:
    # If Help Flag -> Show Instructions: #
    if flag == '-h':
        print 'Example:'
        print '   ./updateInputSIF.py -i inputfile.sif -n nextTemperature.nk'
        print ""
        print 'Example:'
        print '   ./updateInputSIF.py -i inputfile.sif -n nextTemperature.nk -m material'
        print "Program reads the data energy range from 'nextTemperature.nk',"
        print "updates Energy_Range of 'inputfile.sif' accoardingly and "
        print "updates the material:"
        print "   &material"
        print '     Material = "nextTemperature" '
        print "/"
        sys.exit()

    elif flag in ('-i','--ifile'):
        ifile = arg # recieve inputFile

    elif flag in ('-n','--nkfile'):
        nkfile = arg # recieve replacement text
    elif flag in ('-m','--material'):
        material = arg # recieve replacement text



# # # # # # # # # # # # # # # # # # # # # # #  
# # # # #   Read The nk-file Information    #  
# # # # # # # # # # # # # # # # # # # # # # #  
# Read first line of the '.nk'-file, where the data file information is stored:
f = open(nkfile,'r')
nkfileLine1 = f.readline()
f.close()

# Get 'unit' and energy interval [x1,x2]:
nkfileInfo = nkfileLine1.split()
unit = int(nkfileInfo[0])
x1 = float(nkfileInfo[1])
x2 = float(nkfileInfo[2])

# Check unit:
if( unit == 1 ): # eV
    Emin = x1
    Emax = x2

if( unit == 2 ): # micrometer (wavelength)
    #then convert to eV:
    import scipy.constants as sc
    h = sc.h/sc.e # (planck's constant[J s])/(elementary charge) = planck[eV s]
    c = sc.c # speed of light

    Emin = h*c/( x2*(10**(-6)) )
    Emax = h*c/( x1*(10**(-6)) )
    #ref: http://www.pveducation.org/pvcdrom/properties-of-sunlight/energy-of-photon

# REMOVE IF NOT NEEDED OR BETTER FIX OCCURS:
# JUST INCLUDED TO BE SURE THE INTERVALL IS WITHIN THE DATA!
Emin = Emin + Emin*0.01
Emax = Emax - Emax*0.01
print nkfile.split('/')[-1][:-3]+' energy range = ['+str(Emin)+', '+str(Emax)+']'


# # # # # # # # # # # # # # # # # # # # # # #  
# # # # #   Read The nk-file Information    #  
# # # # # # # # # # # # # # # # # # # # # # #  
if( ifile[-3:] == '.nk' ): # To be sure that the '.nk'-file is NOT written to (could destroy the data):
    print "ERROR! You put the '.nk'-file as the input file! This would ruin the '.nk'-file!"
    print "The input file '.sif' file comes after '-i'-flag  and the '.nk'-file comes after"
    print "the '-n'-flag! Be more careful next time..."
    print "                                                 ...program ending"
    sys.exit(2)
else: # Build the new lines to replace the old lines (in the '.sif'-file)
    energyUpdate =   '  Energy_Range =    '+str(Emin)+', '+str(Emax)+'        !(eV) \n'
    materialUpdate = '  Material = "'+nkfile.split('/')[-1][:-3]+'"\n' #if path, split, take last value=filename

    # Open '.sif' file for reading:
    f = open(ifile,'r')
    #filedata = f.read()
    # Read the entire file to memory as array of lines (file small, so this is okay):
    flines = f.readlines() 
    f.close()

    # Find and replace the lines containing energy range and material:
    findex = 0
    for fline in flines:
        if 'Energy_Range' in fline:
            flines[findex] = energyUpdate
        if ('&'+str(material)) in fline:
            flines[findex+1] = materialUpdate
        findex = findex+1

    # Write back the altered lines to the same file:
    f = open(ifile,'w')
    for fline in flines:
        f.write(fline)
    f.close()
