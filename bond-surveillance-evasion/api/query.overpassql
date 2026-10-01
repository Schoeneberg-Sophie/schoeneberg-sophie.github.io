// @name san_francisco_query

[out:json][timeout:150];

// Query specifically targeting surveillance items that have direction or fov tags
(
  node["man_made"="surveillance"]["camera:direction"](37.715,-122.515,37.825,-122.365);
  node["man_made"="surveillance"]["direction"](37.715,-122.515,37.825,-122.365);
  node["man_made"="surveillance"]["camera:fov"](37.715,-122.515,37.825,-122.365);
  way["man_made"="surveillance"]["camera:direction"](37.715,-122.515,37.825,-122.365);
);

out body;
>;
out skel qt;