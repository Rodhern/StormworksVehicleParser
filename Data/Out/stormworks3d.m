% STORMWORKS3D Display graphic parser output overview of polygons.
%    [] = stormworks3d()
%    
%    Load files "X.txt", "Y.txt", Z.txt", "Cidxs.txt" and "Colours.txt" that
%    was exported by the parser.
%    Then use the 'patch' function to paint the faces onto the figure display
%    one at a time.

function [] = stormworks3d ()
  
  % load and scale variables
  load X.txt; load Y.txt; load Z.txt; load Cidxs.txt; load Colours.txt;
  X= X/4; Y= Y/4; Z= Z/4; % position scale is four cubes per meter
  Colours= Colours/255; % RGB values in 1/255th (8 bit)
  
  % a bug in the 'patch' function means it cannot handle NaNs
  for i=2:rows(X),
   js = isnan(X(i,:));
   X(i,js)= X(i-1,js);
   Y(i,js)= Y(i-1,js);
   Z(i,js)= Z(i-1,js);
  endfor
  
  % determine the 'size' of the vehicle and reset the figure display
  xmin = min(min(X)); xmax = max(max(X));
  ymin = min(min(Y)); ymax = max(max(Y));
  zmin = min(min(Z)); zmax = max(max(Z));
  msize = max([xmax-xmin, ymax-ymin, zmax-zmin]) + 0.2; % size for figure display
  dispmin = 0.5*[xmin+xmax-msize, ymin+ymax-msize, zmin+zmax-msize]; % lower axis limits
  dispmax = dispmin + msize; % upper axis limits
  displims = reshape([dispmin; dispmax],1,6);
  clf; axis (displims, "square"); grid on; view(30,15);
  
  % finally, loop through the colours and use 'patch' to draw faces
  for cidx=1:max(Cidxs),
    patch(X(:,Cidxs==cidx),Z(:,Cidxs==cidx),Y(:,Cidxs==cidx),Colours(cidx,:));
  endfor
endfunction
