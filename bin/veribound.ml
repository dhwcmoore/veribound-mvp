open Yojson.Basic
open Yojson.Basic.Util

let usage () =
  print_endline "Usage:";
  print_endline "  veribound verify <sealed.json>";
  print_endline "  veribound verify basel <input.json>";
  exit 1

let read_float_field json field =
  match json |> member field with
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Null -> failwith ("Missing required field: " ^ field)
  | _ -> failwith ("Field is not a number: " ^ field)

let sha256_hex s =
  Digestif.SHA256.digest_string s |> Digestif.SHA256.to_hex

let ensure_dir path =
  if Sys.file_exists path then ()
  else Unix.mkdir path 0o755

let write_pretty_json path json =
  let oc = open_out path in
  output_string oc (Yojson.Basic.pretty_to_string json);
  output_char oc '\n';
  close_out oc

let basel_report_from_input input_path =
  let j = Yojson.Basic.from_file input_path in
  let cet1 = read_float_field j "cet1_capital" in
  let rwa = read_float_field j "risk_weighted_assets" in
  let threshold = read_float_field j "threshold" in

  if rwa <= 0.0 then failwith "risk_weighted_assets must be > 0";

  let ratio = cet1 /. rwa in
  let pass = ratio >= threshold in

  `Assoc [
    ("regime", `String "basel_iii_capital_adequacy");
    ("rule", `String "CET1_ratio >= threshold");
    ("inputs", `Assoc [
      ("cet1_capital", `Float cet1);
      ("risk_weighted_assets", `Float rwa);
      ("threshold", `Float threshold)
    ]);
    ("computed", `Assoc [
      ("cet1_ratio", `Float ratio)
    ]);
    ("status", `String (if pass then "PASS" else "FAIL"))
  ]

let seal_report_results results_json =
  let results_text = Yojson.Basic.to_string results_json in
  let seal_hash = sha256_hex results_text in
  (* The verifier does not validate the signature, it only reports it.
     Keep a stable value for now. *)
  let irrational_signature = Float.pi in

  `Assoc [
    ("results", results_json);
    ("seal_hash", `String seal_hash);
    ("irrational_signature", `Float irrational_signature)
  ]

let cmd_verify_sealed filename =
  let (ok, msg) = Verifier.verify_seal_from_file filename in
  print_endline msg;
  if ok then exit 0 else exit 2

let cmd_verify_basel input_path =
  ensure_dir "results";

  let results_json = basel_report_from_input input_path in
  let sealed = seal_report_results results_json in

  let out_path = "results/basel_report_sealed.json" in
  write_pretty_json out_path sealed;

  print_endline ("Wrote sealed report: " ^ out_path);

  let (ok, msg) = Verifier.verify_seal_from_file out_path in
  print_endline msg;

  if ok then exit 0 else exit 2

let () =
  match Array.to_list Sys.argv with
  | [_; "verify"; "basel"; input_json] ->
      (try cmd_verify_basel input_json with
       | Failure m -> prerr_endline ("❌ " ^ m); exit 2
       | Yojson.Json_error m -> prerr_endline ("❌ JSON error: " ^ m); exit 2
       | exn -> prerr_endline ("❌ Unexpected error: " ^ Printexc.to_string exn); exit 2)
  | [_; "verify"; sealed_json] ->
      cmd_verify_sealed sealed_json
  | _ ->
      usage ()
