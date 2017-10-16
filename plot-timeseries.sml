
structure PlotTimeseries :> sig
    val plot : real vector -> Svg.svg
end = struct

    type state = {
        n : int,
        min : real option,
        max : real option,
        points : Svg.coords list
    }

    fun build (v, { n, min, max, points } : state) =
        let val point = (Real.fromInt n, v)
            val min = SOME (case min of NONE => v
                                      | SOME min => if v < min then v else min)
            val max = SOME (case max of NONE => v
                                      | SOME max => if v > max then v else max)
        in
            { n = n + 1, min = min, max = max, points = point :: points }
        end

    fun plot series =
        let val { n, min, max, points } = 
                Vector.foldl build
                             { n = 0, min = NONE, max = NONE, points = [] }
                             series
            open Svg
            open SvgPathShorthand
        in
            case points of
                [] => { size = (0.0, 0.0), content = [] }
              | first::rest =>
                {
                  size = (Real.fromInt n,
                          case (max, min) of
                              (SOME max, SOME min) => max - min
                            | _ => 0.0),
                  content = [
                      (PATH [M first, L rest],
                       [STROKE (RGB (1.0, 0.0, 0.0)),
                        STROKE_WIDTH 0.1,
                        FILL NO_PAINT])
                  ]
                }
        end
end
          
