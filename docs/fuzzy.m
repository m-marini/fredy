tx = ty = linspace (0, 1, 33)';
[x, y] = meshgrid (tx, ty);

function z = and(x,y)
  z = min(x,y);
endfunction

function z = or(x,y)
  z = max(x,y);
endfunction

function z = not(x)
  z = 1-x;
endfunction

function z = very(x)
  z = x .* x;
endfunction

function z = somewhat(x)
  z = sqrt(x);
endfunction

function z = isTrue(x,y)
  z = min(x, 1 - y);
endfunction

function z = isParadox(x,y)
  z = max(0, x + y - 1);
endfunction

function z = isCertain(x,y)
  z = max(0, 1 - abs(1 - x - y));
endfunction

function z = implies(x,y)
  z = min(1 - x + y, 1);
endfunction

function z = iff(x,y)
  z = 1 - abs(x - y);
endfunction

z = isTrue(x, not(y));

mesh (tx, ty, z);
xlabel ("x");
ylabel ("y");
zlabel ("z");
title ("Function");

