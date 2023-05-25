# Plotly Plots from Haskell

Generates an HTML Document with a Plotly Plot. These are _not_ bindings to anything.

For more details see the [documentation](https://augustunderground.github.io/plotly-hs).

## Example

The [basic line plot example](https://plotly.com/javascript/line-charts/#basic-line-plot)
is recreated like this:

```haskell
import Graphics.Plotly

main :: IO ()
main = plot path script
  where
    script = scatter' [] [[1, 2, 3, 4], [1, 2, 3, 4]]
                         [[10, 15, 13, 17], [16, 5, 11, 9]]
           $ defaultConfig { lineMode = LinesMarkers }
    path   = "path/to/dir/plot.html"
```

Then navigate to `path/to/dir/plot.html` with a web browser.

### More Examples

More examples can be found in `./src/Graphics/Plotly/Example.hs` the results of
which are stored in `./plots`.

## Usage with Stack

Add this to `stack.yaml`:

```yaml
extra-deps:
  - github: AugustUnderground/plotly-hs
    commit: 8cf30ce82ede0b5f70944950a6c8aae3ca18bee4 # Or lates commit
```
