close
clear

in = csvread('out-bench-countcode.csv',1,1);

x = in(1,:);
xs = linspace(min(x),max(x));
codeSizeNOpt = in(2,:);
codeSizeOpt = in(3,:);
codeSize_GHC = in(4,:);
codeSize_eta = in(5,:);

[a x1] = polyfit(x,codeSizeNOpt,1);
[b x2] = polyfit(x,codeSizeOpt,1);
[c x3] = polyfit(x,codeSize_GHC,1);
[d x4] = polyfit(x,codeSize_eta,1);

linFun = @(a,xdata)( a(1)*xdata + a(2) );

[ctx, pLeft, pGHC] = plotyy(x, [codeSizeNOpt', codeSizeOpt', codeSize_eta']...
    ./ 1e3,x, codeSize_GHC ./ 1e6);
pNoOpt = pLeft(1);
pOpt   = pLeft(2);
pEta   = pLeft(3);

pNoOpt.Marker = 'x';
pNoOpt.MarkerSize = 5;
pNoOpt.LineStyle = 'none';

pOpt.Marker = 'x';
pOpt.MarkerSize = 5;
pOpt.LineStyle = 'none';

pEta.Marker = 'x';
pEta.MarkerSize = 5;
pEta.LineStyle = 'none';

hold(ctx(1), 'on');

plot_noOpt = plot(ctx(1), xs, linFun(a,xs) ./ 1e3);
plot_opt = plot(ctx(1), xs, linFun(b,xs) ./ 1e3);
plot_eta = plot(ctx(1), xs, linFun(d,xs) ./ 1e3);

%plot_noOpt.Marker = 'x';
%plot_noOpt.MarkerSize = 5;
plot_noOpt.Color = pNoOpt.Color;
plot_opt.Color = pOpt.Color;
plot_eta.Color = pEta.Color;

%line_fit_jv = plot(xs',expConst(fit_jv,xs'));

%plot_noOpt.Color = ctx(1).YColor;



xlabel('Input size in bytes');
ylabel(ctx(1),'Outputted code size/kB');
ylim(ctx(1),[-1 100]);

hold(ctx(2), 'on');

ylim(ctx(2),[1.649000 1.75]);
ylabel(ctx(2),'Outputted code size/MB');

h(1) = plot_noOpt;
h(2) = plot_opt;
h(3) = plot_eta;
h(4) = pGHC;
l = legend([plot_noOpt, plot_opt, plot_eta, pGHC], 'No Inline', 'Inline', 'eta','GHC','Location','northwest');

legend boxoff

pGHC.Marker = 'x';
pGHC.MarkerSize = 5;


cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/plotCodeSize-out.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
%clear 
%close