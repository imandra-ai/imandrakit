let pp_iter ?(sep = "") ppx out it =
  Fmt.(iter ~sep:(fun out () -> Fmt.fprintf out "%s@ " sep) ppx) out it
