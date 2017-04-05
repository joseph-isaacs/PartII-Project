x = [0.419 0.332 3.188 0.312 0.497 21.78]; % current values for different stages in the pipeline
total = sum (x);
x(2,3) = 0; %Pad with 0 rowx = [1 2 3];
x(2,3) = 0; %Pad with 0 row
h = barh(x,'stack');



set(h(1), 'FaceColor', 'y');
set(h(2), 'FaceColor', 'm');
set(h(3), 'FaceColor', 'b');
set(h(4), 'FaceColor', 'r');
set(h(5), 'FaceColor', 'g');
set(h(6), 'FaceColor', 'c')


set(gca, 'YTickLabelMode', 'Manual');
set(gca, 'YTick', []);

ylim([0.6 1]);
xlim([0 total]);
l = {'Parse' 'Desugar' 'TI' 'SystemF' 'Inline' 'CodeGen'};
legend(l,'FontSize',16);
xlabel('Time (ms)');


matlab2tikz('../../../diss/tex/evaluation/graphs/plotStage.tex',...
    'width' , '\gwidth',...
    'height', '\gheight' );
clear 
close