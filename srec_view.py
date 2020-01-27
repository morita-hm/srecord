
import sys

srecs = []

if __name__ == '__main__':
       with open(sys.argv[1]) as f:
              lines = f.readlines()

       start_addr = int(sys.argv[2], 16)
       stop_addr = start_addr + int(sys.argv[3])

       for l in lines:
              if l[0:2] == 'S3' :
                     len = int(l[2:4], 16) - 5   # data length
                     addr = int(l[4:12], 16)     #
                     for idx in range(len):
                            srecs += [(addr+idx, l[12+2*idx:14+2*idx])]

       val = ''
       for srec in srecs:
              (pos, hexvalue) = srec
              if (start_addr <= pos) and (pos < stop_addr):
                     val += hexvalue
       
       print(val)
             

