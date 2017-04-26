clear;
close;
x = [1 100 199 298 397 496 595 694 793 892 991];

runs = 3;

length = size(x,2);
in = csvread('memory_gc_stack_image_inv-s0.csv',1,0);

% stackGrowth per n for opt
stackH_op = in(1,1:length);

gcTime_op = mean(in(2:runs+1,1:length));
time_op   = mean(in(runs+2:2*runs+1,1:length)) / 1e3;
gcProp_op = gcTime_op ./ time_op;

young_op  = mean(in(2*runs+2:3*runs+1,1:length));
old_op    = mean(in(3*runs+2:4*runs+1,1:length));
meta_op   = mean(in(4*runs+2:5*runs+1,1:length));


memUsed_op      = [young_op; old_op; meta_op];
memUsedTotal_op = sum(memUsed_op);

% stackGrowth per n 
stackH = in(1,length+1:end);

gcTime = mean(in(2:runs+1,length+1:end));
time   = mean(in(runs+2:2*runs+1,length+1:end)) / 1e3;
gcProp = gcTime ./ time;

young = mean(in(2*runs+2:3*runs+1,length+1:end));
old   = mean(in(3*runs+2:4*runs+1,length+1:end));
meta  = mean(in(4*runs+2:5*runs+1,length+1:end));

memUsed = [young; old; meta];
memUsedTotal = sum(memUsed);


%% Final

p = plot (x,[stackH' stackH_op'] /1e3);

hold on;

p2 = plot(0,0);

p2.Color = p(1).Color;

p3 = plot(0,0);

p3.Color = p(2).Color;

p(1).Marker = 'x';
p(1).MarkerSize = 5;
p(2).Marker = 'x';
p(2).MarkerSize = 5;

ylbl = ylabel ('Greatest number of thunks built up/$\cdot 10^{3}$');
xlbl = xlabel ('Input size $n$');
set(xlbl, 'interpreter', 'latex');
set(ylbl, 'interpreter', 'latex');

l = legend([p2, p3], 'No Inlining', 'Inlining', 'Location','northwest');
l.Box = 'off';

cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/plotCallStackGrowth.tex',...
   'width' , '\gwidth',...
   'height', '\gheight' );

%close
%clear
