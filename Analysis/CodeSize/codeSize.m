in = csvread('out-bench-countcode.csv',1,1);

x = in(1,:);
codeSizeNOpt = in(2,:);
codeSizeOpt = in(3,:);
codeSize_GHC = in(4,:);
codeSize_eta = in(5,:);

[a x1] = polyfit(x,codeSizeNOpt,1);
[b x2] = polyfit(x,codeSizeOpt,1);
[c x3] = polyfit(x,codeSize_GHC,1);
[d x4] = polyfit(x,codeSize_eta,1);


ctx = plotyy(x, [codeSizeNOpt', codeSizeOpt', codeSize_eta']...
    ./ 1e3,x, codeSize_GHC ./ 1e6);


l = legend('Inline', 'No Inline', 'eta','GHC','Location','northwest');



xlabel('Input size in bytes');
ylabel(ctx(1),'Outputted code size kB');
ylabel(ctx(2),'Outputted code size MB');

ylim(ctx(2),[1.649000 1.75]);
ylim(ctx(1),[-1 100]);




cleanfigure;
matlab2tikz('../../diss/tex/evaluation/graphs/plotCodeSize-out.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
clear 
close