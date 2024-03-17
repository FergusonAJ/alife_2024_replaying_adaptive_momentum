import random
import sys

if len(sys.argv) != 4:
    print('Error! Expected exactly 2 command line arguments:')
    print('  1. The seed to use')
    print('  2. The input population snapshot')
    print('  3. The output population snapshot')
    exit(1)

random_seed = int(sys.argv[1])
input_filename = sys.argv[2]
output_filename = sys.argv[3]

random.seed(random_seed)

lines = []

with open(input_filename, 'r') as in_fp:
    for line in in_fp:
        line = line.strip()
        if line == '':
            continue
        lines.append(line)

random.shuffle(lines)

with open(output_filename, 'w') as out_fp:
    for line in lines:
        out_fp.write(line + '\n')
