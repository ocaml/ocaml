(* TEST *)

(* Tests for stack-overflow crashes caused by a combinatorial
   explosition in fragile pattern checking. *)

[@@@warning "+4"]

module SyntheticTest = struct
  (* from Luc Maranget *)
  type t = A | B

  let f = function
    | A,A,A,A,A, A,A,A,A,A, A,A,A,A,A, A,A,A -> 1
    | (A|B),(A|B),(A|B),(A|B),(A|B),
      (A|B),(A|B),(A|B),(A|B),(A|B),
      (A|B),(A|B),(A|B),(A|B),(A|B),
      (A|B),(A|B),(A|B) ->  2
end

module RealCodeTest = struct
  (* from Alex Fedoseev *)

  type visibility = Shown | Hidden

  type ('outputValue, 'message) fieldStatus =
  | Pristine
  | Dirty of ('outputValue, 'message) result * visibility

  type message = string

  type fieldsStatuses = {
    iaasStorageConfigurations :
      iaasStorageConfigurationFieldsStatuses array;
  }

  and iaasStorageConfigurationFieldsStatuses = {
    startDate : (int, message) fieldStatus;
    term : (int, message) fieldStatus;
    rawStorageCapacity : (int, message) fieldStatus;
    diskType : (string option, message) fieldStatus;
    connectivityMethod : (string option, message) fieldStatus;
    getRequest : (int option, message) fieldStatus;
    getRequestUnit : (string option, message) fieldStatus;
    putRequest : (int option, message) fieldStatus;
    putRequestUnit : (string option, message) fieldStatus;
    transferOut : (int option, message) fieldStatus;
    transferOutUnit : (string option, message) fieldStatus;
    region : (string option, message) fieldStatus;
    cloudType : (string option, message) fieldStatus;
    description : (string option, message) fieldStatus;
    features : (string array, message) fieldStatus;
    accessTypes : (string array, message) fieldStatus;
    certifications : (string array, message) fieldStatus;
    additionalRequirements : (string option, message) fieldStatus;
  }

  type interface = { dirty : unit -> bool }

  let useForm () = {
    dirty = fun () ->
      Array.for_all
        (fun item ->
          match item with
          | {
              additionalRequirements = Pristine;
              certifications = Pristine;
              accessTypes = Pristine;
              features = Pristine;
              description = Pristine;
              cloudType = Pristine;
              region = Pristine;
              transferOutUnit = Pristine;
              transferOut = Pristine;
              putRequestUnit = Pristine;
              putRequest = Pristine;
              getRequestUnit = Pristine;
              getRequest = Pristine;
              connectivityMethod = Pristine;
              diskType = Pristine;
              rawStorageCapacity = Pristine;
              term = Pristine;
              startDate = Pristine;
            } ->
            false
          | {
              additionalRequirements = Pristine | Dirty (_, _);
              certifications = Pristine | Dirty (_, _);
              accessTypes = Pristine | Dirty (_, _);
              features = Pristine | Dirty (_, _);
              description = Pristine | Dirty (_, _);
              cloudType = Pristine | Dirty (_, _);
              region = Pristine | Dirty (_, _);
              transferOutUnit = Pristine | Dirty (_, _);
              transferOut = Pristine | Dirty (_, _);
              putRequestUnit = Pristine | Dirty (_, _);
              putRequest = Pristine | Dirty (_, _);
              getRequestUnit = Pristine | Dirty (_, _);
              getRequest = Pristine | Dirty (_, _);
              connectivityMethod = Pristine | Dirty (_, _);
              diskType = Pristine | Dirty (_, _);
              rawStorageCapacity = Pristine | Dirty (_, _);
              term = Pristine | Dirty (_, _);
              startDate = Pristine | Dirty (_, _);
            } ->
            true)
        [||]
  }
end
