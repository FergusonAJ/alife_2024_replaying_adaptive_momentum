import sys

if len(sys.argv) != 4:
    print('Error! Expected exactly three command line arguments:')
    print('  1. The output directory to write to')
    print('  2. The snapshot directory to load from')
    print('  3. The number of files to read (num updates)')
    exit(1)

out_dir = sys.argv[1]
snapshot_dir = sys.argv[2]
max_update = int(sys.argv[3])

old_peak = 6
cur_peak = 12
target_peak = 18
second_target_peak = 24
lower_threshold = 10
pop_size = 512

full_data = ''
full_data = 'update'
for i in range(pop_size):
    full_data += ',idx_' + str(i)
full_data += '\n'

with open(out_dir + '/snapshot_data.csv', 'w') as out_fp: 
    header = 'update,leading_edge_index,leading_edge_value,count_under,second_leading_edge_left_index,second_leading_edge_left_val,second_leading_edge_right_index,second_leading_edge_right_val'
    for i in range(old_peak, second_target_peak + 1):
        header += ',count_' + str(i)
    header += ',count_over'
    out_fp.write(header + '\n')
    prev_second_leading_edge_right_index = None
    for update in range(1, max_update + 1):
        #print('Update:', update)
        full_data += str(update)
        with open(snapshot_dir + '/ud_' + str(update) + '.pop', 'r') as in_fp:
            idx = 0
            pop = [0] * pop_size
            num_under = 0
            num_over = 0
            count_map = {}
            leading_edge_index = None
            leading_edge_val = None
            second_leading_edge_left_index = None
            second_leading_edge_left_val = None
            second_leading_edge_right_index = None
            second_leading_edge_right_val = None
            for line in in_fp:
                line = line.strip()
                if line == '':
                    continue
                val = int(line.split(' ')[1])
                if val < old_peak:
                    num_under += 1
                elif val > second_target_peak:
                    num_over += 1
                else:
                    if val not in count_map.keys():
                        count_map[val] = 0
                    count_map[val] += 1
                if val > lower_threshold:
                    leading_edge_index = idx
                    leading_edge_val = val
                if val >= target_peak and second_leading_edge_left_index is None:
                    second_leading_edge_left_index = idx
                    second_leading_edge_left_val = val
                if val >= target_peak - 1 and second_leading_edge_left_index is not None and (prev_second_leading_edge_right_index is None or prev_second_leading_edge_right_index >= idx - 1) :
                    second_leading_edge_right_index = idx
                    second_leading_edge_right_val = val

                pop[idx] = val
                full_data += ',' + str(val)
                idx += 1
            prev_second_leading_edge_right_index = second_leading_edge_right_index
            full_data += '\n'
            data = str(update) + \
                    ',' + str(leading_edge_index) + \
                    ',' + str(leading_edge_val) + \
                    ',' + str(num_under) + \
                    ',' + str(second_leading_edge_left_index) + \
                    ',' + str(second_leading_edge_left_val) + \
                    ',' + str(second_leading_edge_right_index) + \
                    ',' + str(second_leading_edge_right_val)
            for i in range(old_peak, second_target_peak + 1):
                val = 0
                if i in count_map.keys():
                   val = count_map[i] 
                data += ',' + str(val)
            data += ',' + str(num_over)
            out_fp.write(data + '\n')

with open(out_dir + '/full_snapshot_data.csv', 'w') as full_out_fp: 
    full_out_fp.write(full_data)
