% GENERATED FROM docs/music-protocol.org. DO NOT EDIT.
% Portable facts only: SWI-Prolog and the Android Trealla subset can consult
% the same vocabulary without Python implementation details.

music_wire_version(1).
music_page_limit(500).
music_selector_depth_limit(8).
music_selector_clause_limit(64).

music_client_message_type('music.library.query').
music_client_message_type('music.job.submit').
music_client_message_type('music.job.status').
music_client_message_type('music.job.cancel').
music_client_message_type('music.plan.apply').

music_server_message_type('music.library.page').
music_server_message_type('music.job.accepted').
music_server_message_type('music.job.status.ok').
music_server_message_type('music.job.cancel.accepted').
music_server_message_type('music.job.progress').
music_server_message_type('music.job.completed').
music_server_message_type('music.job.failed').
music_server_message_type('music.plan.ready').
music_server_message_type('music.plan.apply.accepted').

music_job_operation(scan, execute).
music_job_operation(hash, execute).
music_job_operation(fingerprint, execute).
music_job_operation(metadata, execute).
music_job_operation(dedupe, execute).
music_job_operation(tag, plan).
music_job_operation(move, plan).
music_job_operation(quarantine, plan).

music_selector_field(id).
music_selector_field(path).
music_selector_field(artist).
music_selector_field(album).
music_selector_field(title).
music_selector_field(genre).
music_selector_field(year).
music_selector_field(format).
music_selector_field(size).
music_selector_field(modified_ns).
music_selector_field(duration_ms).
music_selector_field(recording_id).
music_selector_field(duplicate_class).
music_selector_field(metadata_state).

music_selector_operator(eq).
music_selector_operator(ne).
music_selector_operator(lt).
music_selector_operator(lte).
music_selector_operator(gt).
music_selector_operator(gte).
music_selector_operator(contains).
music_selector_operator(prefix).
music_selector_operator(in).
