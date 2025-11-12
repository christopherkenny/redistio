# Changelog

## redistio 0.0.0.9001

- Full rewrite in `mapgl` instead of `leaflet`. This removes hacks
  around not redrawing polygons.
- Package depends on `mapgl (>= 0.4.1)` to have layer-based filtering,
  which is necessary for fast precinct-level statistics.

## redistio 0.0.0.9000

- Adds interactive district builder with
  [`draw()`](http://www.christophertkenny.com/redistio/reference/draw.md)
  - Adds primary draw panel for interactive redistricting
  - Adds demographics panel for population details by district
  - Adds integrity panel for assessing traditional redistricting
    criteria
  - Adds elections panel for summarizing partisan outcomes
  - Adds algorithms panel for using `redist` algorithms to redraw
    subsets of districts
