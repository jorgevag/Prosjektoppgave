#!/usr/bin/python
import sys, getopt
import fileinput

args = sys.argv[1:]
flags = 'hi:s:r:'

if(args < 3):
    print "Error! Need at least three input values:"
    print 'Example:'
    print '   ./chageText.py -i inputFile.txt -s searchString -r replaceString'
    print "The 'searchString' is the text you want to find in the file 'inputFile.txt'"
    print " and replace by 'replaceString'."
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
        print '   ./chageText.py -i inputFile.txt -s searchString -r replaceString'
        print "The 'searchString' is the text you want to find in the file 'inputFile.txt'"
        print " and replace by 'replaceString'."
        sys.exit()

    elif flag in ('-i','--ifile'):
        ifile = arg # recieve inputFile

    elif flag in ('-s','--sString'):
        sString = arg # recieve search text

    elif flag in ('-r','--rString'):
        rString = arg # recieve replacement text

print "Filename: '"+ifile+"'"
print " ...replacing all instances of '"+str(sString)+"' with '"+str(rString)+"'..."

f = open(ifile,'r')
filedata = f.read()
f.close()

newdata = filedata.replace(sString,rString)

f = open(ifile,'w')
f.write(newdata)
f.close()
