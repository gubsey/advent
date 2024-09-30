$grid = []

ARGF.each_line do |e|
    row = []
    e.each_byte do |b|
        case b.chr
        when '.'
            row.push 0
        when '#'
            row.push 1
        end
    end
    $grid.push row
end

hist = {}
hist[$grid.hash] = true

def next_frame
    $grid.each_index do |y|
        $grid[y].each_index do |x|
            dirs = []

            if y > 0 then dirs.push [x,y-1] end
            if y < $grid.length then dirs.push [x,y+1] end
            if x > 0 then dirs.push [x-1,y] end
            if y < $grid[y].length then dirs.push [x+1,y] end

            puts [x,y].inspect, dirs.inspect
        end
    end
end

next_frame
