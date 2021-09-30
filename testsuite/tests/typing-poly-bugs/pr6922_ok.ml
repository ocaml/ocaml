(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module Order = struct
    module type Total = sig
        type t
        val compare: t -> t -> int
    end
end

module type Profile = sig
    module Priority: Order.Total

    class type ['level] prioritizer = object
        method code: 'level -> Priority.t
        method tag: 'level -> string
    end

    class ['level] event:
        'level #prioritizer -> 'level -> string ->
        object
            method prioritizer: 'level prioritizer
            method level: 'level
            method message: string
        end

    class type ['event] archiver = object
        constraint 'event = 'level #event
        method emit: 'event -> unit
    end

    class virtual ['archiver] agent:
        'level #prioritizer -> 'level -> 'archiver list ->
        object
            constraint 'event = 'level #event
            constraint 'archiver = 'event #archiver
            val mutable archivers_: 'archiver list
            val mutable limit_: Priority.t
            method virtual private event: 'level -> string -> 'event
            method setlimit: 'level -> unit
            method enabled: 'level -> bool
            method private put: 'a 'b. 'level -> ('event -> 'b) -> ('a, unit, string, string, string, 'b) format6 -> 'a
        end
end

module Create(P: Order.Total) = struct
    module Priority = P

    class type ['level] prioritizer = object
        method code: 'level -> Priority.t
        method tag: 'level -> string
    end

    class ['level] event prioritizer level message =
        let prioritizer = (prioritizer :> 'level prioritizer) in
        object
            method prioritizer = prioritizer
            method level: 'level = level
            method message: string = message
        end

    class type ['event] archiver = object
        constraint 'event = 'level #event
        method emit: 'event -> unit
    end

    class virtual ['archiver] agent prioritizer limit archivers =
        let _ = (prioritizer :> 'level prioritizer) in
        let _ = (archivers :> 'archiver list) in
        object(self:'self)
            constraint 'event = 'level #event
            constraint 'archiver = 'event #archiver

            val mutable archivers_ = archivers
            val mutable limit_ = prioritizer#code limit

            method virtual private event: 'level -> string -> 'event

            method setlimit limit = limit_ <- prioritizer#code limit
            method enabled limit = prioritizer#code limit >= limit_

            method private put:
                type a b. 'level -> ('event -> b) ->
                (a, unit, string, string, string, b) format6 -> a
                = fun level cont ->
                    let f message =
                        let e = self#event level message in
                        if self#enabled level then
                            List.iter (fun j -> j#emit e) archivers_;
                        cont e
                    in
                    Printf.kprintf f
        end
end

module Basic = struct
    include Create(struct type t = int  let compare a b = b - a end)

    type invalid = [ `Invalid ]
    type fail = [ `Fail ]
    type error = [ `Error ]
    type warn = [ `Warn ]
    type notice = [ `Notice ]
    type info = [ `Info ]
    type debug = [ `Debug ]

    type basic = [ invalid | fail | error | warn | notice | info | debug ]
    type enable = [ `None | `All ]
    type level = [ basic | enable ]
end

class ['level] basic_prioritizer =
    object(_:'self)
        constraint 'self = 'level #Basic.prioritizer
        constraint 'level = [> Basic.level ]

        method code = function
            | `All -> max_int
            | `Invalid -> 7000
            | `Fail -> 6000
            | `Error -> 5000
            | `Warn -> 4000
            | `Notice -> 3000
            | `Info -> 2000
            | `Debug -> 1000
            | `None -> min_int
            | _ -> invalid_arg "Oni_cf_journal: no code defined for priority!"

        method tag =
            let invalid_ = "INVALID" in
            let fail_ = "FAIL" in
            let error_ = "ERROR" in
            let warn_ = "WARN" in
            let notice_ = "NOTICE" in
            let info_ = "INFO" in
            let debug_ = "DEBUG" in
            function
            | `Invalid -> invalid_
            | `Fail -> fail_
            | `Error -> error_
            | `Warn -> warn_
            | `Notice -> notice_
            | `Info -> info_
            | `Debug -> debug_
            | _ -> invalid_arg "Oni_cf_journal: no tag defined for priority!"
    end

class ['event] basic_channel_archiver channel = object
    constraint 'self = 'event #Basic.archiver
    constraint 'level = [> Basic.level ]
    constraint 'event = 'level #Basic.event

    method channel = channel

    method emit e =
        let _ = (e :> 'event) in
        let n = e#level in
        let p = e#prioritizer in
        if (p#code `Fail) - (p#code e#level) > 0 then begin
            let tag = p#tag n in
            let m = e#message in
            Printf.fprintf channel "%s: %s\n" tag m;
            flush channel
        end
end

class virtual ['archiver] basic_agent prioritizer limit archivers =
    let _ = (prioritizer :> 'level basic_prioritizer) in
    (*
    let _ = (limit : 'level) in
    let _ = (archivers : 'archiver list) in
    *)
    object(self)
        constraint 'level = [> Basic.level ]
        constraint 'event = 'level #Basic.event
        constraint 'archiver = 'event #Basic.archiver
        inherit ['archiver] Basic.agent prioritizer limit archivers (* as super *)

        (*
        method! private put:
            'a 'b. 'level -> ('event -> 'b) ->
            ('a, unit, string, 'b) format4 -> 'a = super#put
        *)

        method invalid:
            'a 'b. ('a, unit, string, string, string, 'b) format6 -> 'a =
            self#put `Invalid (fun x -> invalid_arg x#message)

        method fail:
            'a 'b. ('a, unit, string, string, string, 'b) format6 -> 'a =
            self#put `Fail (fun x -> failwith x#message)

        method error:
            'a. ('a, unit, string, string, string, unit) format6 -> 'a =
            self#put `Error ignore

        method warn:
            'a. ('a, unit, string, string, string, unit) format6 -> 'a =
            self#put `Warn ignore

        method notice:
            'a. ('a, unit, string, string, string, unit) format6 -> 'a =
            self#put `Notice ignore

        method info:
            'a. ('a, unit, string, string, string, unit) format6 -> 'a =
            self#put `Info ignore

        method debug:
            'a. ('a, unit, string, string, string, bool) format6 -> 'a =
            self#put `Debug (fun _ -> true)
    end
