

let init () =
  OUnitShared.mutex_create :=
  (fun () ->
     let mutex = Mutex.create () in
     {
       OUnitShared.
       lock = (fun () -> Mutex.lock mutex);
       try_lock = (fun () -> Mutex.try_lock mutex);
       unlock = (fun () -> Mutex.unlock mutex);
     });
  OUnitRunnerThreads.init ()
