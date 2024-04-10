import pygame
import os

# Config options
show_frontend = True # If true, we show the realtime app, if false only run once
# These ones shouldn't need changed...
data_filename = os.path.join('..', 'data', 'full_snapshot_data.csv')
color_filename = os.path.join('..', 'data', 'color_data.txt')
plot_dir = os.path.join('..', 'plots')

class Handler:
    def __init__(self, data_filename, color_filename, plot_dir, show_frontend = True):
        self.data_filename = data_filename
        self.color_filename = color_filename
        self.plot_dir = plot_dir
        self.show_frontend = show_frontend
        self.are_colors_valid = False
        self.init_pygame()
        self.load_colors()
        self.load_data()
        self.generate_muller_plot()
        if self.show_frontend:
            self.run()

    def init_pygame(self):
        pygame.init()
        self.screen_width = 768
        self.screen_height = 512
        if self.show_frontend:
            self.screen = pygame.display.set_mode((self.screen_width, self.screen_height))
        else:
            self.screen = pygame.Surface((self.screen_width, self.screen_height))

    def hex_to_rgb(self, s):
        s = s.strip()
        while s[0] == '#': # Lop off # prefixes
            s = s[1:]
        s = s.strip()
        if len(s) != 6:
            print('Error! Invalid hex value:', s)
        r = int(s[0:2], base = 16)
        g = int(s[2:4], base = 16)
        b = int(s[4:6], base = 16)
        return (r, g, b)
    
    def load_colors(self):
        self.color_map = {}
        index = 5
        line_counter = 0
        with open(self.color_filename, 'r') as fp:
            for line in fp:
                if '//' in line:
                    line = line[:line.find('//')]
                line = line.strip()
                if line == '':
                    continue
                self.color_map[index] = self.hex_to_rgb(line)
                index += 1
                line_counter += 1
        if line_counter != 21:
            print('Unexpected number of lines in color file!')
            print('Expected 21, found', line_counter)
            self.are_colors_valid = False
        else:
            self.are_colors_valid = True
    
    def load_data(self):
        self.data_map = {}
        with open(self.data_filename, 'r') as fp:
            for line in fp:
                line = line.strip()
                if line == '':
                    continue
                if 'update' in line: # Skip the header
                    continue
                line_parts = line.split(',')
                if len(line_parts) != 513:
                    print('Unexpected line found in data')
                update = int(line_parts[0])
                if update < 769: # Trim off our one extra generation
                    self.data_map[update] = line_parts[1:]
        if len(self.data_map.keys()) != 768:
            print('Unexpected number of lines in data file!')
            print('Expected 768, found', len(self.data_map.keys()))
            print(self.data_map.keys())

    def generate_muller_plot(self):
        self.screen.fill((0,0,0))
        if self.are_colors_valid:
            for update in range(1, 768):
                for org_idx in range(0, 512):
                    org_val = int(self.data_map[update][org_idx])
                    if org_val < 5:
                        org_val = 5
                    if org_val > 25:
                        org_val = 25
                    color = self.color_map[org_val]
                    self.screen.set_at((update, 512 - org_idx), color)
            output_filename = os.path.join(self.plot_dir, 'muller.png')
            pygame.image.save(self.screen, output_filename)
            print('Saved Muller plot to', output_filename)
        else:
            print('Did not generate image because colors are invalid!')


    def run(self):
        self.is_done = False
        while not self.is_done:
            event_list = pygame.event.get()
            for event in event_list:
                if event.type == pygame.QUIT:
                    self.is_done = True
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_q or event.key == pygame.K_ESCAPE:
                        self.is_done = True
                    elif event.key == pygame.K_SPACE:
                        self.load_colors()
                        self.load_data()
                        self.generate_muller_plot()
            pygame.display.flip()

if __name__ == '__main__':
    handler = Handler(data_filename, color_filename, plot_dir, show_frontend)
    pygame.quit()
