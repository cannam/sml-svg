
structure SvgShorten : sig

              (** Take a path containing lots of absolute move/line
                  segments, and convert it to an equivalent path using
                  relative segments and horizontal/vertical commands
                  for cases where that seems it will shorten the
                  result. *)
              val shortenPath : Svg.path_element list ->
                                Svg.path_element list

              (** As shortenPath, but also round to the nearest
                  integer any value within epsilon of it. (shortenPath
                  rounds values only if they are within 1.0e~8.) *)
              val shortenPathWith : { epsilon : real } ->
                                    Svg.path_element list ->
                                    Svg.path_element list

          end = struct

    open Svg
                                            
    fun absTarget form =
        case form of
            MOVE_TO c => SOME c
          | LINE_TO [c] => SOME c
          | LINE_TO _ => raise Fail "LINE_TO should have been exploded"
          | HORIZONTAL_TO h => SOME (h, 0.0)
          | VERTICAL_TO v => SOME (0.0, v)
          | CUBIC_TO [] => NONE
          | CUBIC_TO cc => SOME (#target (hd (rev cc)))
          | CUBIC_SMOOTH_TO [] => NONE
          | CUBIC_SMOOTH_TO cc => SOME (#target (hd (rev cc)))
          | QUADRATIC_TO [] => NONE
          | QUADRATIC_TO cc => SOME (#target (hd (rev cc)))
          | QUADRATIC_SMOOTH_TO [] => NONE
          | QUADRATIC_SMOOTH_TO cc => SOME (#target (hd (rev cc)))
          | ARC_TO [] => NONE
          | ARC_TO cc => SOME (#target (hd (rev cc)))
          | CLOSE_PATH =>
            raise Fail "CLOSE_PATH should have been handled separately"

    fun relativeTo (x, y) (x', y') = (x + x', y + y')
                  
    fun relTarget pos form =
        case form of
            MOVE_TO c => SOME (relativeTo pos c)
          | LINE_TO [c] => SOME (relativeTo pos c)
          | LINE_TO _ => raise Fail "LINE_TO should have been exploded"
          | HORIZONTAL_TO h => SOME (relativeTo pos (h, 0.0))
          | VERTICAL_TO v => SOME (relativeTo pos (0.0, v))
          | CUBIC_TO _ => NONE
          | CUBIC_SMOOTH_TO _ => NONE
          | QUADRATIC_TO _ => NONE
          | QUADRATIC_SMOOTH_TO _ => NONE
          | ARC_TO _ => NONE
          | CLOSE_PATH =>
            raise Fail "CLOSE_PATH should have been handled separately"

    fun withTarget (c as (x, y)) form =
        case form of
            MOVE_TO _ => MOVE_TO c
          | LINE_TO _ => LINE_TO [c]
          | HORIZONTAL_TO _ => HORIZONTAL_TO x
          | VERTICAL_TO _ => VERTICAL_TO y
          | _ => 
            raise Fail "this form should have been handled already"

    fun isCloseToInt epsilon r = Real.abs (r - Real.realRound r) < epsilon
    fun roundIfClose epsilon r = if isCloseToInt epsilon r
                                 then Real.realRound r
                                 else r
                  
    fun pick epsilon (xabs, yabs) (xrel, yrel) =
        let val round' = roundIfClose epsilon
            val (rxr, ryr) = (round' xrel, round' yrel)
        in
            if Real.!= (rxr, xrel) orelse Real.!= (ryr, yrel)
            then REL (rxr, ryr)
            else let val (rxa, rya) = (round' xabs, round' yabs)
                 in
                     if Real.!= (rxa, xabs) orelse Real.!= (rya, yabs)
                     then ABS (rxa, rya)
                     else REL (rxr, ryr)
                 end
        end
                  
    fun shortenFolder epsilon =
        let val pick' = pick epsilon
        in
            fn (elt, (acc, posMaybe)) =>
               case elt of
                   REL form => ((elt :: acc),
                                case posMaybe of
                                    NONE => NONE
                                  | SOME pos => relTarget pos form)
                 | ABS form =>
                   let val newPosMaybe = case form of
                                             CLOSE_PATH => NONE
                                           | _ => absTarget form
                       val adjustedElt =
                           case (posMaybe, newPosMaybe, form) of
                               (NONE, _, _) => elt
                             | (_, NONE, _) => elt
                             | (_, _, CUBIC_TO _) => elt
                             | (_, _, CUBIC_SMOOTH_TO _) => elt
                             | (_, _, QUADRATIC_TO _) => elt
                             | (_, _, QUADRATIC_SMOOTH_TO _) => elt
                             | (_, _, ARC_TO _) => elt
                             | (SOME (x, y), _, LINE_TO [(x', y')]) =>
                               (if Real.== (y, y')
                                then REL (HORIZONTAL_TO (x'-x))
                                else if Real.== (x, x')
                                then REL (VERTICAL_TO (y'-y))
                                else case pick' (x', y') (x'-x, y'-y) of
                                         ABS (x, y) => elt
                                       | REL (x, y) => REL (LINE_TO [(x, y)]))
                             | (SOME (x, y), SOME (x', y'), form) =>
                               case pick' (x', y') (x'-x, y'-y) of
                                   ABS (x, y) => elt
                                 | REL (x, y) => REL (withTarget (x, y) form)
                   in
                       (adjustedElt :: acc, newPosMaybe)
                   end
        end

    fun implodeHorizVert (path : Svg.path_element list)
        : Svg.path_element list =
        case foldl (fn (elt, (prev, eltAcc)) =>
                       case (prev, elt) of
                           (ABS (HORIZONTAL_TO h1), ABS (HORIZONTAL_TO h2)) =>
                           (elt, eltAcc)
                         | (REL (HORIZONTAL_TO h1), REL (HORIZONTAL_TO h2)) =>
                           (REL (HORIZONTAL_TO (h1 + h2)), eltAcc)
                         | (ABS (VERTICAL_TO h1), ABS (VERTICAL_TO h2)) =>
                           (elt, eltAcc)
                         | (REL (VERTICAL_TO h1), REL (VERTICAL_TO h2)) =>
                           (REL (VERTICAL_TO (h1 + h2)), eltAcc)
                         | (prev, elt) =>
                           (elt, prev :: eltAcc))
                   (hd path, [])
                   (tl path) of
            (prev, eltAcc) => rev (prev :: eltAcc)
                
    fun implodeLines (path : Svg.path_element list)
        : Svg.path_element list =
        case foldl (fn (elt, (prev, pointAcc, eltAcc)) =>
                       case (prev, elt) of
                           (ABS (LINE_TO [c1]), ABS (LINE_TO c2)) =>
                           (elt, c1 :: pointAcc, eltAcc)
                         | (REL (LINE_TO [c1]), REL (LINE_TO c2)) =>
                           (elt, c1 :: pointAcc, eltAcc)
                         | (ABS (LINE_TO [c]), _) =>
                           (elt, [], ABS (LINE_TO (rev (c :: pointAcc)))
                                     :: eltAcc)
                         | (REL (LINE_TO [c]), _) => 
                           (elt, [], REL (LINE_TO (rev (c :: pointAcc)))
                                     :: eltAcc)
                         | (prev, elt) =>
                           (elt, [], prev :: eltAcc))
                   (hd path, [], [])
                   (tl path) of
            (prev, [], eltAcc) => rev (prev :: eltAcc)
          | (ABS (LINE_TO [c]), pointAcc, eltAcc) =>
            rev (ABS (LINE_TO (rev (c :: pointAcc))) :: eltAcc)
          | (REL (LINE_TO [c]), pointAcc, eltAcc) =>
            rev (REL (LINE_TO (rev (c :: pointAcc))) :: eltAcc)
          | _ => raise Fail "unexpected non-empty pointAcc when last element was not a line"
        
    fun explodeLines (path : Svg.path_element list)
        : Svg.path_element list =
        List.concat (map (fn (ABS (LINE_TO ll)) =>
                             map (fn c => ABS (LINE_TO [c])) ll
                           | (REL (LINE_TO ll)) =>
                             map (fn c => REL (LINE_TO [c])) ll
                           | other => [other])
                         path)
                
    fun shortenPathWith { epsilon : real } (path : Svg.path_element list)
        : Svg.path_element list =
        case path of
            [] => []
          | path =>
            case foldl (shortenFolder epsilon)
                       ([], NONE)
                       (explodeLines path) of
                (acc, _) =>
                implodeHorizVert (implodeLines (rev acc))
                
    fun shortenPath (path : Svg.path_element list)
        : Svg.path_element list =
        shortenPathWith { epsilon = 1E~8 } path

end
