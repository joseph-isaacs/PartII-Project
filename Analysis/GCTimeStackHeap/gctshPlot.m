runs = 10;

x = [50,65125,171054,213197,238381,255941,269245,279861,288640,296091,...
     302541,308213,313263,317806,321929,325698,329166];
xs = linspace(min(x),max(x));

length = size(x,2);
in = csvread('mem_usageplus-newCalc.csv',1,0);

% stackGrowth per n for opt
stackH_op = 5.0253;

gcTime_op = mean(in(1:runs,1:length));
time_op   = mean(in(runs+1:2*runs,1:length)) / 1e3;
gcProp_op = gcTime_op ./ time_op;

young_op  = mean(in(2*runs+1+2:3*runs,1:length));
old_op    = mean(in(3*runs+1:4*runs,1:length));
meta_op   = mean(in(4*runs+1:5*runs,1:length));


memUsed_op      = [young_op; old_op; meta_op];
memUsedTotal_op = sum(memUsed_op);

% stackGrowth per n 
stackH = 11.0418;

gcTime = mean(in(1:runs,length+1:end));
time   = mean(in(runs+1:2*runs,length+1:end)) / 1e3;
gcProp = gcTime ./ time;

young = mean(in(2*runs+1:3*runs,length+1:end));
old   = mean(in(3*runs+1:4*runs,length+1:end));
meta  = mean(in(4*runs+1:5*runs,length+1:end));

memUsed = [young; old; meta];
memUsedTotal = sum(memUsed);

%% Mem Used

p = plot(x ./ 1e4,[memUsedTotal' memUsedTotal_op'] ./ 1e3);

hold on;

p(1).Marker = 'x';

p(1).MarkerSize = 5;

p(2).Marker = 'x';
p(2).MarkerSize = 5;

linFun = @(a,xdata)( a(1)*xdata + a(2) );

[nop a] = polyfit(x,memUsedTotal,1);
[op b] = polyfit(x,memUsedTotal_op,1);

plotLine = plot(xs' ./ 1e4,[linFun(nop,xs)' linFun(op,xs)'] ./ 1e3);

plotLine(1).Color = p(1).Color;
plotLine(2).Color = p(2).Color;
p(1).LineStyle = 'none';
p(2).LineStyle = 'none';

hold on;

xlbl = xlabel('Input value $n$ to \texttt{testN}/$\cdot 10^{3}$');
set(xlbl, 'interpreter', 'latex');
ylabel('Peak memory usage/MB');

l = legend(plotLine, 'No Inlining','Inlining','Location','northwest');
l.Box = 'off'

matlab2tikz('../../diss/tex/evaluation/graphs/memUsed.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );


%% gc_time
close

plot(x,gcTime);
hold on;
plot(x,gcTime_op);

legend('Nop','Op','Location','northwest');

%matlab2tikz('../tex/gcTime_plot.tex',  'width', '\fwidth' ); 





%% Mem Used in different generation non opt
close

area(x,memUsed');

legend('Young','Par','meta','Location','northwest');

%matlab2tikz('../tex/differentMemAreas.tex',  'width', '\fwidth' ); 


%% Mem Used in different generation opt
close

area(x,memUsed_op');

legend('Young','Par','meta','Location','northwest');

%matlab2tikz('../tex/differentMemAreasOp.tex',  'width', '\fwidth' ); 

%% GC proportion
close 

plot(x,time_op - gcTime_op);
hold on;

plot(x,time- gcTime);


legend('t-gc opt','t-gc','Location','northwest');

ylabel('Time to run minus time in GC');
xlabel('Input size (n)');

%matlab2tikz('../tex/runTimeSubGC.tex',  'width', '\fwidth' ); 


%% GC and mem usage
close

area(x,memUsed' / 1e4);
hold on;
plot(x,gcTime * 1e1);
plot(x,time);
plot(x, (stackH/norm(x)) .* x );


%% GC op mem usage
close


area(x,memUsed_op' / 1e4);
hold on;
plot(x,gcTime_op * 1e1);
plot(x,time_op);
plot(x, (stackH_op/norm(x)) .* x );


%% Final

close



