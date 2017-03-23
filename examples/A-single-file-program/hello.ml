let () =
  Random.self_init ();

  if Random.bool () then
    print_endline "Hi."
  else
    print_endline "Hello."
