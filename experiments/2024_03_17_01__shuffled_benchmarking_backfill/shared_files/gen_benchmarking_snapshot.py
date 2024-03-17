import random
import sys

if len(sys.argv) != 6:
    print('Error! Expected exactly 5 command line arguments:')
    print('  1. The index of the leading edge')
    print('  2. The value of the leading edge')
    print('  3. The number of extra organisms in the leading edge')
    print('  4. The population size')
    print('  5. The output population snapshot file')
    exit(1)

leading_edge_index = int(sys.argv[1])
leading_edge_val = int(sys.argv[2])
leading_edge_extra_orgs = int(sys.argv[3])
pop_size = int(sys.argv[4])
output_filename = sys.argv[5]

with open(output_filename, 'w') as out_fp:
    for i in range(pop_size):
        offset = leading_edge_index - i
        if offset <= leading_edge_extra_orgs and offset >= 0:
            out_fp.write('[ ' + str(leading_edge_val) + ' ]\n')
        elif offset < 0:
            out_fp.write('[ 6 ]\n')
        else:
            out_fp.write('[ 12 ]\n')


