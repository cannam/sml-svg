                                 
open Svg
open SvgPathShorthand
         
val mypath = [ M (1.0, 2.0),
               l [(3.0, 0.0), (~1.0, 1.5)],
               Z
             ]

val mysvg = {
    size = (10.0, 5.0),
    content = [
        (PATH mypath, [STROKE (RGB (1.0, 0.0, 0.0)),
                       STROKE_WIDTH 0.2,
                       FILL (COLOUR "darkslategray")]),
        (RECT { origin = (4.0, 4.0), size = (2.0, 1.0) },
         [ STROKE_WIDTH 0.1,
           STROKE (COLOUR "pink"),
           FILL (COLOUR "purple") ]),
        (GROUP [(PATH [M (3.0, 4.0), H 0.0, L [(0.0, 0.0)]],
                 [STROKE (RGB (0.0, 0.0, 1.0)), FILL NO_PAINT]),
                (POLYLINE [(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 1.0), (0.0, 0.0)],
                 [STROKE (RGB (0.0, 1.0, 0.0)), STROKE_WIDTH 0.1, FILL NO_PAINT]),
                (TEXT { origin = (0.0, 5.0),
                        rotation = 0.0,
                        text = "<Help/> me!&@!" }, [FONT_FAMILY ["Helvetica"], FONT_SIZE 2.0])],
         [TRANSFORM (ROTATE 45.0)])
    ]}

fun sinusoid (nsamples, samplerate, { frequency, phase }) =
    Vector.tabulate (nsamples,
                     fn i =>
                        let val x = Real.fromInt i
                        in
                            Math.sin (phase + (x * 2.0 * Math.pi * frequency /
                                               samplerate))
                        end)

val wave = sinusoid (2000, 1000.0, { frequency = 50.0, phase = 0.0 })

                    (*                    
fun main () = print (SvgSerialise.serialiseDocument (PlotTimeseries.plot wave))

                    *)

fun main () =
    print (SvgSerialise.serialiseDocument mysvg)
          
