;;; Crate sources for ty, generated with
;;; guix import crate --lockfile=<ruff>/Cargo.lock ty
;;; from the ruff submodule of ty 0.0.109.  Regenerate on every ty bump.

(define-module (zaijab packages rust-crates)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs)
  ) ; define-module

(define rust-adler2-2.0.1
  (crate-source "adler2" "2.0.1"
                "1ymy18s9hs7ya1pjc9864l30wk8p2qfqdi7mhhcc5nfakxbij09j"))

(define rust-aho-corasick-1.1.5
  (crate-source "aho-corasick" "1.1.5"
                "1fhjkp2nbs7gg4y1b68hpc8028rpax8aiscfh9b60q78m4pn90n9"))

(define rust-alloca-0.4.0
  (crate-source "alloca" "0.4.0"
                "1x6p4387rz6j7h342kp3b7bgvqzyl9mibf959pkfk9xflrgd19z5"))

(define rust-allocator-api2-0.2.21
  (crate-source "allocator-api2" "0.2.21"
                "08zrzs022xwndihvzdn78yqarv2b9696y67i6h78nla3ww87jgb8"))

(define rust-anes-0.1.6
  (crate-source "anes" "0.1.6"
                "16bj1ww1xkwzbckk32j2pnbn5vk6wgsl3q4p3j9551xbcarwnijb"))

(define rust-annotate-snippets-0.11.5
  (crate-source "annotate-snippets" "0.11.5"
                "1i1bmr5vy957l8fvivj9x1xs24np0k56rdgwj0bxqk45b2p8w3ki"))

(define rust-anstream-0.6.21
  (crate-source "anstream" "0.6.21"
                "0jjgixms4qjj58dzr846h2s29p8w7ynwr9b9x6246m1pwy0v5ma3"))

(define rust-anstream-1.0.0
  (crate-source "anstream" "1.0.0"
                "13d2bj0xfg012s4rmq44zc8zgy1q8k9yp7yhvfnarscnmwpj2jl2"))

(define rust-anstyle-1.0.14
  (crate-source "anstyle" "1.0.14"
                "0030szmgj51fxkic1hpakxxgappxzwm6m154a3gfml83lq63l2wl"))

(define rust-anstyle-lossy-1.1.4
  (crate-source "anstyle-lossy" "1.1.4"
                "07x0kqkklc0124cbn49fc21d9wzp9w2vhaw827md113ghbfablq4"))

(define rust-anstyle-parse-0.2.7
  (crate-source "anstyle-parse" "0.2.7"
                "1hhmkkfr95d462b3zf6yl2vfzdqfy5726ya572wwg8ha9y148xjf"))

(define rust-anstyle-parse-1.0.0
  (crate-source "anstyle-parse" "1.0.0"
                "03hkv2690s0crssbnmfkr76kw1k7ah2i6s5amdy9yca2n8w7zkjj"))

(define rust-anstyle-query-1.1.4
  (crate-source "anstyle-query" "1.1.4"
                "1qir6d6fl5a4y2gmmw9a5w93ckwx6xn51aryd83p26zn6ihiy8wy"))

(define rust-anstyle-svg-0.1.11
  (crate-source "anstyle-svg" "0.1.11"
                "1pighxng51vldrs47r0k0vqvxqzaqiy3syklz6qa3bbfjy6frf96"))

(define rust-anstyle-wincon-3.0.10
  (crate-source "anstyle-wincon" "3.0.10"
                "0ajz9wsf46a2l3pds7v62xbhq2cffj7wrilamkx2z8r28m0k61iy"))

(define rust-anyhow-1.0.104
  (crate-source "anyhow" "1.0.104"
                "0w34jjcm02p5g9kvsjr1dvpw0zs2fi7igi6nr414fkm5gz85w2ik"))

(define rust-approx-0.5.1
  (crate-source "approx" "0.5.1"
                "1ilpv3dgd58rasslss0labarq7jawxmivk17wsh8wmkdm3q15cfa"))

(define rust-ar-archive-writer-0.5.3
  (crate-source "ar_archive_writer" "0.5.3"
                "1dhjwdapx0ydx38r7aj8w4rqmdhxs1xl2zp8xala0h11zzg5ikbk"))

(define rust-arc-swap-1.9.3
  (crate-source "arc-swap" "1.9.3"
                "0vmfc2wjnn7gyjfawvnq6a5pxsbiq1rbzzmra0p6969vv6rn429q"))

(define rust-argfile-1.0.0
  (crate-source "argfile" "1.0.0"
                "1rsk3z37qs27cq09s6wrn3kkqld88qqj9755pwq2j3ga7mrrlj4r"))

(define rust-arrayvec-0.7.8
  (crate-source "arrayvec" "0.7.8"
                "0mmd8lrijbvg1qp4c5zis5dq41a3mjv2rb6bxkyj9kwaw2k6gyyk"))

(define rust-assert-fs-1.1.4
  (crate-source "assert_fs" "1.1.4"
                "0dw144dmhpjv2ba2pz00zfk0r5kaapq3dsdw401gidq7r9q5rkvf"))

(define rust-async-trait-0.1.89
  (crate-source "async-trait" "0.1.89"
                "1fsxxmz3rzx1prn1h3rs7kyjhkap60i7xvi0ldapkvbb14nssdch"))

(define rust-attribute-derive-0.11.0
  (crate-source "attribute-derive" "0.11.0"
                "0ryisgalxj44dlv1x9l56mxdyqrvqwscjhd3kp75fik8d1xfzyfj"))

(define rust-attribute-derive-macro-0.11.0
  (crate-source "attribute-derive-macro" "0.11.0"
                "164fzcfyfikja6q8z2fz9p4q53shsnyljilfpcjr2m3kbrp4439h"))

(define rust-autocfg-1.5.0
  (crate-source "autocfg" "1.5.0"
                "1s77f98id9l4af4alklmzq46f21c980v13z2r1pcxx6bqgw0d1n0"))

(define rust-bit-set-0.8.0
  (crate-source "bit-set" "0.8.0"
                "18riaa10s6n59n39vix0cr7l2dgwdhcpbcm97x1xbyfp1q47x008"))

(define rust-bit-vec-0.8.0
  (crate-source "bit-vec" "0.8.0"
                "1xxa1s2cj291r7k1whbxq840jxvmdsq9xgh7bvrxl46m80fllxjy"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-bitflags-2.13.2
  (crate-source "bitflags" "2.13.2"
                "01hbgjwvid66850fzi76mvn5f2bqycx6sf165ng1kfjqq9bl1v9x"))

(define rust-bitvec-1.1.1
  (crate-source "bitvec" "1.1.1"
                "0dqq44v9877q0xbl4g6aaaf3xhh3m1ca7ag0iy4l17ap5k8w7knx"))

(define rust-block-buffer-0.10.4
  (crate-source "block-buffer" "0.10.4"
                "0w9sa2ypmrsqqvc20nhwr75wbb5cjr4kkyhpjm1z1lv2kdicfy1h"))

(define rust-block2-0.6.2
  (crate-source "block2" "0.6.2"
                "1xcfllzx6c3jc554nmb5qy6xmlkl6l6j5ib4wd11800n0n3rvsyd"))

(define rust-boxcar-0.2.14
  (crate-source "boxcar" "0.2.14"
                "0vksx6zjnkqwxsm2bp21vhmc35dqlmhjgzr69cdxm10awkm4pxin"))

(define rust-bstr-1.13.1
  (crate-source "bstr" "1.13.1"
                "0pxyrnp8nb2iwcbadzird7xr2awrbjzi2jwqx47f4i22q531pcvb"))

(define rust-bumpalo-3.19.0
  (crate-source "bumpalo" "3.19.0"
                "0hsdndvcpqbjb85ghrhska2qxvp9i75q2vb70hma9fxqawdy9ia6"))

(define rust-bytecheck-0.8.2
  (crate-source "bytecheck" "0.8.2"
                "12yasgcsxapxnpiv2ac3jzvmc69zy3gj7isss4ch9jpdq2i37ahc"))

(define rust-bytecheck-derive-0.8.2
  (crate-source "bytecheck_derive" "0.8.2"
                "1ncjsg08a6kawhxhy24rpxn287yclbprbc7034i1v0ninn15wf49"))

(define rust-bytes-1.12.0
  (crate-source "bytes" "1.12.0"
                "14xmxm8imyvw675bsgyadmzm9k63js1sdqh7099p0hlj2p9zbqwa"))

(define rust-cachedir-0.3.1
  (crate-source "cachedir" "0.3.1"
                "0wyqx30crm2qsq4ny57hhljyq6iw6j4qfg7fbfiqznvpf29z60s7"))

(define rust-camino-1.2.6
  (crate-source "camino" "1.2.6"
                "14g55g2pn08yb29rqq8nqdbp4cial62x1bn87jga652c9c7d7fmv"))

(define rust-cast-0.3.0
  (crate-source "cast" "0.3.0"
                "1dbyngbyz2qkk0jn2sxil8vrz3rnpcj142y184p9l4nbl9radcip"))

(define rust-castaway-0.2.4
  (crate-source "castaway" "0.2.4"
                "0nn5his5f8q20nkyg1nwb40xc19a08yaj4y76a8q2y3mdsmm3ify"))

(define rust-cc-1.2.65
  (crate-source "cc" "1.2.65"
                "15iv1nizwngnq9if3id4cjjs4pl0rnjkd6xm82vcq5vwpv4ywa72"))

(define rust-cfg-if-1.0.4
  (crate-source "cfg-if" "1.0.4"
                "008q28ajc546z5p2hcwdnckmg0hia7rnx52fni04bwqkzyrghc4k"))

(define rust-cfg-aliases-0.2.1
  (crate-source "cfg_aliases" "0.2.1"
                "092pxdc1dbgjb6qvh83gk56rkic2n2ybm4yvy76cgynmzi3zwfk1"))

(define rust-chacha20-0.10.0
  (crate-source "chacha20" "0.10.0"
                "00bn2rn8l68qvlq93mhq7b4ns4zy9qbjsyjbb9kljgl4hqr9i3bg"))

(define rust-char-str-0.0.4
  (crate-source "char_str" "0.0.4"
                "1i2hzw6k9xczam7f2ysxbdsikc1laihrgwhaw485cldvba5agjy9"))

(define rust-ciborium-0.2.2
  (crate-source "ciborium" "0.2.2"
                "03hgfw4674im1pdqblcp77m7rc8x2v828si5570ga5q9dzyrzrj2"))

(define rust-ciborium-io-0.2.2
  (crate-source "ciborium-io" "0.2.2"
                "0my7s5g24hvp1rs1zd1cxapz94inrvqpdf1rslrvxj8618gfmbq5"))

(define rust-ciborium-ll-0.2.2
  (crate-source "ciborium-ll" "0.2.2"
                "1n8g4j5rwkfs3rzfi6g1p7ngmz6m5yxsksryzf5k72ll7mjknrjp"))

(define rust-clap-4.6.7
  (crate-source "clap" "4.6.7"
                "0il98y2rfw75984ck59znd4n592p07bxz8yy3a9blddb02rpd25a"))

(define rust-clap-builder-4.6.7
  (crate-source "clap_builder" "4.6.7"
                "0kbhai5rv1vj9r4np52g2b9fmhvy3y82digs9v40c57bgbxrf1zc"))

(define rust-clap-complete-4.5.58
  (crate-source "clap_complete" "4.5.58"
                "0jmg0idg96cvx51l35ypia1np3q7sfj5wqxvi7kjs59fmlr0pgvm"))

(define rust-clap-complete-command-0.6.1
  (crate-source "clap_complete_command" "0.6.1"
                "0qhv99j7msqyw7j17hswqwpqbdvqawy8l7ip6rnnh5930n61k3ns"))

(define rust-clap-complete-nushell-4.5.8
  (crate-source "clap_complete_nushell" "4.5.8"
                "1kixnzc8rjqjhk189s1jjvg24v21d1ymj7a2knzna7k9jhb9a30a"))

(define rust-clap-derive-4.6.7
  (crate-source "clap_derive" "4.6.7"
                "1q36bgbkfb9rdz7jcvd0ffv0wwlyw0lg3z6iwdcybm0mjjvm3izr"))

(define rust-clap-lex-1.0.0
  (crate-source "clap_lex" "1.0.0"
                "0c8888qi1l9sayqlv666h8s0yxn2qc6jr88v1zagk43mpjjjx0is"))

(define rust-clearscreen-5.0.0
  (crate-source "clearscreen" "5.0.0"
                "1cqpxd9l7x4bxph07xw5g90iadggk6fm6kd04mzf3r1pn8c9kgkf"))

(define rust-codspeed-6.0.0
  (crate-source "codspeed" "6.0.0"
                "015mkcwkwkdsp5idc7024h2jbz15qn1wsi97bx9qwywkgf022z3y"))

(define rust-codspeed-criterion-compat-6.0.0
  (crate-source "codspeed-criterion-compat" "6.0.0"
                "054wkqj80hxdypwczv17bb0n20qiia563ppnzfyq4qg4z6f9654a"))

(define rust-codspeed-criterion-compat-walltime-6.0.0
  (crate-source "codspeed-criterion-compat-walltime" "6.0.0"
                "1n515r0zjvmfl2f22fm1yfwvm8hpsvhla6k67rkay5rdlxrmay5g"))

(define rust-codspeed-divan-compat-6.0.0
  (crate-source "codspeed-divan-compat" "6.0.0"
                "1k0jkd3l2c6f594v2020kxxk39wkx3qlvca4864hprjmwp24hznc"))

(define rust-codspeed-divan-compat-macros-6.0.0
  (crate-source "codspeed-divan-compat-macros" "6.0.0"
                "16a7n7q908dl39qfma0wj264qbr0ndqcdfnw32dvgijcimx2mdhy"))

(define rust-codspeed-divan-compat-walltime-6.0.0
  (crate-source "codspeed-divan-compat-walltime" "6.0.0"
                "18ri1dni11mbrnij15gfbc55s8b4lpifgkknp4a646spjxxfgi0m"))

(define rust-collection-literals-1.0.2
  (crate-source "collection_literals" "1.0.2"
                "1y0xm4lrnk2qdjkf41fs2xi8kc711wwkmlpp76iqps5qixdzdcr6"))

(define rust-colorchoice-1.0.4
  (crate-source "colorchoice" "1.0.4"
                "0x8ymkz1xr77rcj1cfanhf416pc4v681gmkc9dzb3jqja7f62nxh"))

(define rust-colored-3.1.1
  (crate-source "colored" "3.1.1"
                "0d5cpbgvyvmmky199s885s6385ykd75q6qg3d2kcxjxq563ldygs"))

(define rust-compact-str-0.10.0
  (crate-source "compact_str" "0.10.0"
                "05i4pld14qqp6q9mj3q6ksa0wl12cvddlb4b02bqpd9vqc4dmz3r"))

(define rust-condtype-1.3.0
  (crate-source "condtype" "1.3.0"
                "1by78npyhkc30jccc7kirvwip1fj0jhi2bwfmcw44dqz81xa1w5s"))

(define rust-console-0.16.6
  (crate-source "console" "0.16.6"
                "1qv4mgi3kyvasw15mj9lcn14skrqg6m4vbxmm0kc24scfxb4jsp9"))

(define rust-console-error-panic-hook-0.1.7
  (crate-source "console_error_panic_hook" "0.1.7"
                "1g5v8s0ndycc10mdn6igy914k645pgpcl8vjpz6nvxkhyirynsm0"))

(define rust-console-log-1.1.0
  (crate-source "console_log" "1.1.0"
                "1w8fdj8p5zbz7qwn3bmy07dcyzn110945m2lrxn3bf9p7vprr4c6"))

(define rust-constant-time-eq-0.6.1
  (crate-source "constant_time_eq" "0.6.1"
                "1nbi63dr76nlkx8z0d409rkarl20x6239rykrjix0rfzfnqzxdly"))

(define rust-countme-3.0.1
  (crate-source "countme" "3.0.1"
                "0dn62hhvgmwyxslh14r4nlbvz8h50cp5mnn1qhqsw63vs7yva13p"))

(define rust-cpufeatures-0.2.17
  (crate-source "cpufeatures" "0.2.17"
                "10023dnnaghhdl70xcds12fsx2b966sxbxjq5sxs49mvxqw5ivar"))

(define rust-cpufeatures-0.3.0
  (crate-source "cpufeatures" "0.3.0"
                "00fjhygsqmh4kbxxlb99mcsbspxcai6hjydv4c46pwb67wwl2alb"))

(define rust-crc32fast-1.5.0
  (crate-source "crc32fast" "1.5.0"
                "04d51liy8rbssra92p0qnwjw8i9rm9c4m3bwy19wjamz1k4w30cl"))

(define rust-criterion-0.8.2
  (crate-source "criterion" "0.8.2"
                "1wwq9pfildrkqgb5pq3mwmv297kvvsizkx7m6sjzk4i4mar4c04m"))

(define rust-criterion-plot-0.5.0
  (crate-source "criterion-plot" "0.5.0"
                "1c866xkjqqhzg4cjvg01f8w6xc1j3j7s58rdksl52skq89iq4l3b"))

(define rust-criterion-plot-0.8.2
  (crate-source "criterion-plot" "0.8.2"
                "1si9mrnzgs0123mr6d5pmhxq29rxph2q6pbvwjal6mav9wphmn6q"))

(define rust-crossbeam-0.8.5
  (crate-source "crossbeam" "0.8.5"
                "07nz85g99gyf8f1m4pznzllzi61fsg6s96grq9x5ywh7i36hc577"))

(define rust-crossbeam-channel-0.5.15
  (crate-source "crossbeam-channel" "0.5.15"
                "1cicd9ins0fkpfgvz9vhz3m9rpkh6n8d3437c3wnfsdkd3wgif42"))

(define rust-crossbeam-deque-0.8.6
  (crate-source "crossbeam-deque" "0.8.6"
                "0l9f1saqp1gn5qy0rxvkmz4m6n7fc0b3dbm6q1r5pmgpnyvi3lcx"))

(define rust-crossbeam-epoch-0.9.18
  (crate-source "crossbeam-epoch" "0.9.18"
                "03j2np8llwf376m3fxqx859mgp9f83hj1w34153c7a9c7i5ar0jv"))

(define rust-crossbeam-queue-0.3.12
  (crate-source "crossbeam-queue" "0.3.12"
                "059igaxckccj6ndmg45d5yf7cm4ps46c18m21afq3pwiiz1bnn0g"))

(define rust-crossbeam-utils-0.8.21
  (crate-source "crossbeam-utils" "0.8.21"
                "0a3aa2bmc8q35fb67432w16wvi54sfmb69rk9h5bhd18vw0c99fh"))

(define rust-crunchy-0.2.4
  (crate-source "crunchy" "0.2.4"
                "1mbp5navim2qr3x48lyvadqblcxc1dm0lqr0swrkkwy2qblvw3s6"))

(define rust-crypto-common-0.1.6
  (crate-source "crypto-common" "0.1.6"
                "1cvby95a6xg7kxdz5ln3rl9xh66nz66w46mm3g56ri1z5x815yqv"))

(define rust-csv-1.4.0
  (crate-source "csv" "1.4.0"
                "0f7r2ip0rbi7k377c3xmsh9xd69sillffhpfmbgnvz3yrxl9vkaj"))

(define rust-csv-core-0.1.12
  (crate-source "csv-core" "0.1.12"
                "0gfrjjlfagarhyclxrqv6b14iaxgvgc8kmwwdvw08racvaqg60kx"))

(define rust-ctrlc-3.5.2
  (crate-source "ctrlc" "3.5.2"
                "0qh1lvlr6k58dliqllx1n7mjfwp1mzr607bks3r9m0a5msrgmcg0"))

(define rust-dashmap-6.2.1
  (crate-source "dashmap" "6.2.1"
                "1705w9fx4g30287dx2b0xlmy86l29hnvipba2y5cfq920rf1sdp6"))

(define rust-datatest-stable-0.3.3
  (crate-source "datatest-stable" "0.3.3"
                "10nxz8m2xaffwqz04hgfqnwv6p5l4mx3h9jliakg775n5qrdfrx8"))

(define rust-defmt-1.1.0
  (crate-source "defmt" "1.1.0"
                "0zvzzimxq1zmw3gj42pi326ghihyzhf7pf3w4cyrb8chci829rd6"))

(define rust-defmt-macros-1.1.0
  (crate-source "defmt-macros" "1.1.0"
                "02vaaxgbifai8kchn7nnixyzfa6qyj0q4qwbknkikxy8x5q7g8ph"))

(define rust-defmt-parser-1.0.0
  (crate-source "defmt-parser" "1.0.0"
                "0gpfky9sssil5qfaix5wxcwiqk7snszhl5gq3vcwkrxjncs07mhh"))

(define rust-diff-0.1.13
  (crate-source "diff" "0.1.13"
                "1j0nzjxci2zqx63hdcihkp0a4dkdmzxd7my4m7zk6cjyfy34j9an"))

(define rust-difflib-0.4.0
  (crate-source "difflib" "0.4.0"
                "1s7byq4d7jgf2hcp2lcqxi2piqwl8xqlharfbi8kf90n8csy7131"))

(define rust-digest-0.10.7
  (crate-source "digest" "0.10.7"
                "14p2n6ih29x81akj097lvz7wi9b6b9hvls0lwrv7b6xwyy0s5ncy"))

(define rust-dirs-6.0.0
  (crate-source "dirs" "6.0.0"
                "0knfikii29761g22pwfrb8d0nqpbgw77sni9h2224haisyaams63"))

(define rust-dirs-sys-0.5.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "dirs-sys" "0.5.0"
                "1aqzpgq6ampza6v012gm2dppx9k35cdycbj54808ksbys9k366p0"))

(define rust-dispatch2-0.3.0
  (crate-source "dispatch2" "0.3.0"
                "1v1ak9w0s8z1g13x4mj2y5im9wmck0i2vf8f8wc9l1n6lqi9z849"))

(define rust-displaydoc-0.2.5
  (crate-source "displaydoc" "0.2.5"
                "1q0alair462j21iiqwrr21iabkfnb13d6x5w95lkdg21q2xrqdlp"))

(define rust-divan-macros-0.1.17
  (crate-source "divan-macros" "0.1.17"
                "0z5n3f4m5c7flqnr6vg9qz51j9mjb1s2afcsfnqf7x9nwsc1vicd"))

(define rust-drop-bomb-0.1.5
  (crate-source "drop_bomb" "0.1.5"
                "1qc59a53ngwxpnbvl8xidp2cmwrl671dhbzw7zijmjjaq0hqxnlv"))

(define rust-dunce-1.0.5
  (crate-source "dunce" "1.0.5"
                "04y8wwv3vvcqaqmqzssi6k0ii9gs6fpz96j5w9nky2ccsl23axwj"))

(define rust-dyn-clone-1.0.20
  (crate-source "dyn-clone" "1.0.20"
                "0m956cxcg8v2n8kmz6xs5zl13k2fak3zkapzfzzp7pxih6hix26h"))

(define rust-either-1.15.0
  (crate-source "either" "1.15.0"
                "069p1fknsmzn9llaizh77kip0pqmcwpdsykv2x30xpjyija5gis8"))

(define rust-encode-unicode-1.0.0
  (crate-source "encode_unicode" "1.0.0"
                "1h5j7j7byi289by63s3w4a8b3g6l5ccdrws7a67nn07vdxj77ail"))

(define rust-equivalent-1.0.2
  (crate-source "equivalent" "1.0.2"
                "03swzqznragy8n0x31lqc78g2af054jwivp7lkrbrc0khz74lyl7"))

(define rust-errno-0.3.14
  (crate-source "errno" "0.3.14"
                "1szgccmh8vgryqyadg8xd58mnwwicf39zmin3bsn63df2wbbgjir"))

(define rust-escape8259-0.5.3
  (crate-source "escape8259" "0.5.3"
                "1in2zrdak794x8v11lkrrgyqxjjqbijvg0yfn2paay0rb9xxv4jn"))

(define rust-escargot-0.5.15
  (crate-source "escargot" "0.5.15"
                "1kyhkrxdbxlw839h3l838ck5ir96i9v2p9x6kh650yy95fisxhqi"))

(define rust-etcetera-0.11.0
  (crate-source "etcetera" "0.11.0"
                "15myc4rl62iah8acdl1sxmrdxb8ci55zbphrv07s55qx3i6wqj6y"))

(define rust-fancy-regex-0.14.0
  (crate-source "fancy-regex" "0.14.0"
                "162j2qx2ikgl79grq12mawyflwkirnjzrvxh11a1xbmwjidcn93f"))

(define rust-fastrand-2.3.0
  (crate-source "fastrand" "2.3.0"
                "1ghiahsw1jd68df895cy5h3gzwk30hndidn3b682zmshpgmrx41p"))

(define rust-fern-0.7.1
  (crate-source "fern" "0.7.1"
                "0a9v59vcq2fgd6bwgbfl7q6b0zzgxn85y6g384z728wvf1gih5j3"))

(define rust-filetime-0.2.29
  (crate-source "filetime" "0.2.29"
                "0napyyfccb26r7fyh9hg7ixrh4vph9h7y7k4iv1j19phqwrpla2w"))

(define rust-find-msvc-tools-0.1.9
  (crate-source "find-msvc-tools" "0.1.9"
                "10nmi0qdskq6l7zwxw5g56xny7hb624iki1c39d907qmfh3vrbjv"))

(define rust-flate2-1.1.2
  (crate-source "flate2" "1.1.2"
                "07abz7v50lkdr5fjw8zaw2v8gm2vbppc0f7nqm8x3v3gb6wpsgaa"))

(define rust-fnv-1.0.7
  (crate-source "fnv" "1.0.7"
                "1hc2mcqha06aibcaza94vbi81j6pr9a1bbxrxjfhc91zin8yr7iz"))

(define rust-foldhash-0.1.5
  (crate-source "foldhash" "0.1.5"
                "1wisr1xlc2bj7hk4rgkcjkz3j2x4dhd1h9lwk7mj8p71qpdgbi6r"))

(define rust-foldhash-0.2.0
  (crate-source "foldhash" "0.2.0"
                "1nvgylb099s11xpfm1kn2wcsql080nqmnhj1l25bp3r2b35j9kkp"))

(define rust-form-urlencoded-1.2.2
  (crate-source "form_urlencoded" "1.2.2"
                "1kqzb2qn608rxl3dws04zahcklpplkd5r1vpabwga5l50d2v4k6b"))

(define rust-fs-err-3.3.0
  (crate-source "fs-err" "3.3.0"
                "1h08mdjhdv3c48j3m32kj487rk6lwv3f5j6jrw1h14pwvd9f1zbk"))

(define rust-fsevent-sys-4.1.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "fsevent-sys" "4.1.0"
                "1liz67v8b0gcs8r31vxkvm2jzgl9p14i78yfqx81c8sdv817mvkn"))

(define rust-funty-2.0.0
  (crate-source "funty" "2.0.0"
                "177w048bm0046qlzvp33ag3ghqkqw4ncpzcm5lq36gxf2lla7mg6"))

(define rust-futures-core-0.3.32
  (crate-source "futures-core" "0.3.32"
                "07bbvwjbm5g2i330nyr1kcvjapkmdqzl4r6mqv75ivvjaa0m0d3y"))

(define rust-futures-task-0.3.32
  (crate-source "futures-task" "0.3.32"
                "14s3vqf8llz3kjza33vn4ixg6kwxp61xrysn716h0cwwsnri2xq3"))

(define rust-futures-util-0.3.32
  (crate-source "futures-util" "0.3.32"
                "1mn60lw5kh32hz9isinjlpw34zx708fk5q1x0m40n6g6jq9a971q"))

(define rust-gen-lsp-types-0.11.0
  (crate-source "gen-lsp-types" "0.11.0"
                "05lilj7w32sgarvw1sjprn11b1vnp1njk9rmx5x450w07an8fj5n"))

(define rust-generic-array-0.14.7
  (crate-source "generic-array" "0.14.7"
                "16lyyrzrljfq424c3n8kfwkqihlimmsg5nhshbbp48np3yjrqr45"))

(define rust-get-size-derive2-0.11.1
  (crate-source "get-size-derive2" "0.11.1"
                "0bl09af2cvc962v03bncp4yjvgd6s1chygjjc3gk7486g6hxmw18"))

(define rust-get-size2-0.11.1
  (crate-source "get-size2" "0.11.1"
                "0nij0d15svgpn3xrnznwbzj7a1d3rya6v4bl1dzsrgwy6l3ms2h5"))

(define rust-getrandom-0.2.16
  (crate-source "getrandom" "0.2.16"
                "14l5aaia20cc6cc08xdlhrzmfcylmrnprwnna20lqf746pqzjprk"))

(define rust-getrandom-0.3.4
  (crate-source "getrandom" "0.3.4"
                "1zbpvpicry9lrbjmkd4msgj3ihff1q92i334chk7pzf46xffz7c9"))

(define rust-getrandom-0.4.2
  (crate-source "getrandom" "0.4.2"
                "0mb5833hf9pvn9dhvxjgfg5dx0m77g8wavvjdpvpnkp9fil1xr8d"))

(define rust-glob-0.3.4
  (crate-source "glob" "0.3.4"
                "02zby4rsidb2ksrnysyrsaap7rk6wpp7vl5chflndafhl5gaisz4"))

(define rust-globset-0.4.20
  (crate-source "globset" "0.4.20"
                "1249r63326pzaz6z1c04mmdp6dqg6z3kni47jyylans622a4mhq7"))

(define rust-globwalk-0.9.1
  (crate-source "globwalk" "0.9.1"
                "0mz7bsa66p2rrgnz3l94ac4kbklh7mq8j30iizyxjy4qyvmn1xqb"))

(define rust-half-2.6.0
  (crate-source "half" "2.6.0"
                "1j83v0xaqvrw50ppn0g33zig0zsbdi7xiqbzgn7sd5al57nrd4a5"))

(define rust-hashbrown-0.14.5
  (crate-source "hashbrown" "0.14.5"
                "1wa1vy1xs3mp11bn3z9dv0jricgr6a2j0zkf1g19yz3vw4il89z5"))

(define rust-hashbrown-0.15.5
  (crate-source "hashbrown" "0.15.5"
                "189qaczmjxnikm9db748xyhiw04kpmhm9xj9k9hg0sgx7pjwyacj"))

(define rust-hashbrown-0.17.1
  (crate-source "hashbrown" "0.17.1"
                "0jmqz7i4yl6cm7rbn0i2ffkfrmwi6xkmzkaldr2v8bcsx2v0jngd"))

(define rust-hashlink-0.12.0
  (crate-source "hashlink" "0.12.0"
                "0fssgkd0bzyk7fm8fi88iciag84xpfv7hisbxabfxbfp9qk1y255"))

(define rust-heck-0.5.0
  (crate-source "heck" "0.5.0"
                "1sjmpsdl8czyh9ywl3qcsfsq9a307dg4ni2vnlwgnzzqhc4y0113"))

(define rust-hermit-abi-0.5.2
  (crate-source "hermit-abi" "0.5.2"
                "1744vaqkczpwncfy960j2hxrbjl1q01csm84jpd9dajbdr2yy3zw"))

(define rust-html-escape-0.2.13
  (crate-source "html-escape" "0.2.13"
                "0xml3hswv0205fbm5iq7dqiwjkr6d245xkfppwi7wqjdfr4x86kd"))

(define rust-icu-collections-2.3.0
  (crate-source "icu_collections" "2.3.0"
                "04x59h6vdq0cnpippim1nr471ivlsnnn470sj1d5v864h48d4s7s"))

(define rust-icu-locale-core-2.3.0
  (crate-source "icu_locale_core" "2.3.0"
                "1sqdj16wwl7h9y6r7j394av4kpdb7zryz9h169ffwbm9imc2hvnm"))

(define rust-icu-normalizer-2.3.0
  (crate-source "icu_normalizer" "2.3.0"
                "0vv43ixk2wmbxrx7kl33cwkhx1wdyb1q3pa18qkyshan4dgwzy8j"))

(define rust-icu-normalizer-data-2.3.0
  (crate-source "icu_normalizer_data" "2.3.0"
                "1811h0ppb7lwq1q2492p5x6lcmlwmhbmkf69fhyvzcz0scgdlqqm"))

(define rust-icu-properties-2.3.0
  (crate-source "icu_properties" "2.3.0"
                "0j51hi8qgf0l6a7qzvnwsc61598w2fpnsklicld6ci9immva4z3y"))

(define rust-icu-properties-data-2.3.0
  (crate-source "icu_properties_data" "2.3.0"
                "1akw1gp5rcaiz377xzsnkx8f92qax4kh3lfn9y4rcjj6q4wg1475"))

(define rust-icu-provider-2.3.1
  (crate-source "icu_provider" "2.3.1"
                "0wrydhwprwgyka3r3sw6276syjnlw6fpqr2zsm2srvxv7afvnyyj"))

(define rust-id-arena-2.3.0
  (crate-source "id-arena" "2.3.0"
                "0m6rs0jcaj4mg33gkv98d71w3hridghp5c4yr928hplpkgbnfc1x"))

(define rust-idna-1.1.0
  (crate-source "idna" "1.1.0"
                "1pp4n7hppm480zcx411dsv9wfibai00wbpgnjj4qj0xa7kr7a21v"))

(define rust-idna-adapter-1.2.1
  (crate-source "idna_adapter" "1.2.1"
                "0i0339pxig6mv786nkqcxnwqa87v4m94b2653f6k3aj0jmhfkjis"))

(define rust-ignore-0.4.33
  (crate-source "ignore" "0.4.33"
                "0bs0d0qy8n6n2r4s01s234vr33ngssb1sm0rgnmcb7bjxlrridh0"))

(define rust-imara-diff-0.2.0
  (crate-source "imara-diff" "0.2.0"
                "0p2wmak4pbqfa93fihply18kq8q0nxg6zl0dhampipv6yxid809g"))

(define rust-imperative-1.0.7
  (crate-source "imperative" "1.0.7"
                "0w1r4xaks94ydv4d0y262lm8avkq24m153mdkbjm4p2pkjyx1q9m"))

(define rust-indexmap-2.14.2
  (crate-source "indexmap" "2.14.2"
                "0mf86hbjkkcd82cpq683bblbs0zwa8ndla96ci8p1ji6bl7ijknc"))

(define rust-indicatif-0.18.6
  (crate-source "indicatif" "0.18.6"
                "037vk2cr5b0iwri5023wjyww7d4gqpjcf8f0g6x1mv5lsrn80cwl"))

(define rust-indoc-2.0.8
  (crate-source "indoc" "2.0.8"
                "1lc4agds9bqf3lin6kkz2jgmahaqwrkaq1fkij5fpzvcg68jcyx3"))

(define rust-inotify-0.11.0
  (crate-source "inotify" "0.11.0"
                "1wq8m657rl085cg59p38sc5y62xy9yhhpvxbkd7n1awi4zzwqzgk"))

(define rust-inotify-sys-0.1.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "inotify-sys" "0.1.5"
                "1syhjgvkram88my04kv03s0zwa66mdwa5v7ddja3pzwvx2sh4p70"))

(define rust-insta-1.49.0
  (crate-source "insta" "1.49.0"
                "1k91zskbkqjj97rhb7dkpms9py6i6c868frz1g0qcjvy50p3szdn"))

(define rust-insta-cmd-0.7.0
  (crate-source "insta-cmd" "0.7.0"
                "00qlqicnbnvg08xlgn189l3rl1yd0c9pqpak042cz45k3npz9zdz"))

(define rust-interpolator-0.5.0
  (crate-source "interpolator" "0.5.0"
                "1n6rnk7s61zf20fxjv8yxqfxvl2d68zdrs7i2631w4mf38cm5pbi"))

(define rust-intrusive-collections-0.10.1
  (crate-source "intrusive-collections" "0.10.1"
                "0q19p7vy6s6c0qhx6vmvvhd78nh0sbzrwf7qd99cndmsbs9nbqc0"))

(define rust-inventory-0.3.24
  (crate-source "inventory" "0.3.24"
                "16y3vbab2ld8ykjap1xxwk001jliyqsj8np57zpcrx7jfq6c7w54"))

(define rust-is-macro-0.3.8
  (crate-source "is-macro" "0.3.8"
                "1sc8qgjb789l15gzlfj8fh62864av2xn75jz07rr8m7205halrw2"))

(define rust-is-terminal-0.4.16
  (crate-source "is-terminal" "0.4.16"
                "1acm63whnpwiw1padm9bpqz04sz8msymrmyxc55mvlq8hqqpykg0"))

(define rust-is-terminal-polyfill-1.70.1
  (crate-source "is_terminal_polyfill" "1.70.1"
                "1kwfgglh91z33kl0w5i338mfpa3zs0hidq5j4ny4rmjwrikchhvr"))

(define rust-itertools-0.10.5
  (crate-source "itertools" "0.10.5"
                "0ww45h7nxx5kj6z2y6chlskxd1igvs4j507anr6dzg99x1h25zdh"))

(define rust-itertools-0.13.0
  (crate-source "itertools" "0.13.0"
                "11hiy3qzl643zcigknclh446qb9zlg4dpdzfkjaa9q9fqpgyfgj1"))

(define rust-itertools-0.14.0
  (crate-source "itertools" "0.14.0"
                "118j6l1vs2mx65dqhwyssbrxpawa90886m3mzafdvyip41w2q69b"))

(define rust-itertools-0.15.0
  (crate-source "itertools" "0.15.0"
                "1p412hriqm4kxn7x1539vz2p5c5s1v2m36m4kis2ai4dyn9syjwb"))

(define rust-itoa-1.0.15
  (crate-source "itoa" "1.0.15"
                "0b4fj9kz54dr3wam0vprjwgygvycyw8r0qwg7vp19ly8b2w16psa"))

(define rust-jiff-0.2.38
  (crate-source "jiff" "0.2.38"
                "10x38b9qb1b47mq54d9j4ljkpk7i5ygvknn0lf4h1sybbmqhbc5j"))

(define rust-jiff-core-0.1.0
  (crate-source "jiff-core" "0.1.0"
                "02axx56pkh2w4bw5rp94qlvcpwzd3n2w2025fnikvrgg762aiv3z"))

(define rust-jiff-static-0.2.38
  (crate-source "jiff-static" "0.2.38"
                "0p39a035j9c634llmr8lh1jz7ml8gqrbsla4d3j7wz6gadr83j9c"))

(define rust-jiff-tzdb-0.1.4
  (crate-source "jiff-tzdb" "0.1.4"
                "09350bna4vxdn2fv7gd08ay41llkflmfyvpx5d6l088axc2kfa61"))

(define rust-jiff-tzdb-platform-0.1.3
  (crate-source "jiff-tzdb-platform" "0.1.3"
                "1s1ja692wyhbv7f60mc0x90h7kn1pv65xkqi2y4imarbmilmlnl7"))

(define rust-jobserver-0.1.34
  (crate-source "jobserver" "0.1.34"
                "0cwx0fllqzdycqn4d6nb277qx5qwnmjdxdl0lxkkwssx77j3vyws"))

(define rust-jod-thread-1.0.0
  (crate-source "jod-thread" "1.0.0"
                "094v0brn9l4qcs6hf21r666q6x9vnl0za494qh7ivpi8gpdysdx0"))

(define rust-js-sys-0.3.100
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "js-sys" "0.3.100"
                "0qi1wjakyw2rx9wwprcfx77g3lvn1b8n6yvfhj2pgym4swh5y0pj"))

(define rust-kqueue-1.1.1
  (crate-source "kqueue" "1.1.1"
                "0sjrsnza8zxr1zfpv6sa0zapd54kx9wlijrz9apqvs6wsw303hza"))

(define rust-kqueue-sys-1.0.4
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "kqueue-sys" "1.0.4"
                "12w3wi90y4kwis4k9g6fp0kqjdmc6l00j16g8mgbhac7vbzjb5pd"))

(define rust-lazy-static-1.5.0
  (crate-source "lazy_static" "1.5.0"
                "1zk6dqqni0193xg6iijh7i3i44sryglwgvx20spdvwk3r6sbrlmv"))

(define rust-leb128fmt-0.1.0
  (crate-source "leb128fmt" "0.1.0"
                "1chxm1484a0bly6anh6bd7a99sn355ymlagnwj3yajafnpldkv89"))

(define rust-libc-0.2.191
  (crate-source "libc" "0.2.191"
                "02i4j7pjh129y92rijaia4576131fa2xjsii1dc3c1fnlv4qgkb5"))

(define rust-libcst-1.9.0
  (crate-source "libcst" "1.9.0"
                "18q2c6bsfvyz22c8mr6jnz89nv1v9mffz5lmz0rw7afbc2f8vdi1"))

(define rust-libcst-derive-1.9.0
  (crate-source "libcst_derive" "1.9.0"
                "1hl9glic4z278zdw7vy22a7k7pf3256yaf7il8439p7vndpsr8ap"))

(define rust-libm-0.2.16
  (crate-source "libm" "0.2.16"
                "10brh0a3qjmbzkr5mf5xqi887nhs5y9layvnki89ykz9xb1wxlmn"))

(define rust-libmimalloc-sys-0.1.49
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libmimalloc-sys" "0.1.49"
                "1sdqq31sbf8dbdng8fsyzl2c5xxphn6dvr6ggik6zhg18cpsaiba"))

(define rust-libredox-0.1.10
  (crate-source "libredox" "0.1.10"
                "1jswil4ai90s4rh91fg8580x8nikni1zl3wnch4h01nvidqpwvs1"))

(define rust-libtest-mimic-0.8.1
  (crate-source "libtest-mimic" "0.8.1"
                "0cwxnbv3wl00xsz7q0sf5xzksnqadqw8995a6g7a9pcyy4p9d5sj"))

(define rust-libz-rs-sys-0.5.5
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "libz-rs-sys" "0.5.5"
                "05blwx9jn7nkcwzx1iiy0kac0w18vxchwyf72zd27vjwh3kh21f1"))

(define rust-linux-raw-sys-0.12.1
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "linux-raw-sys" "0.12.1"
                "0lwasljrqxjjfk9l2j8lyib1babh2qjlnhylqzl01nihw14nk9ij"))

(define rust-litemap-0.8.0
  (crate-source "litemap" "0.8.0"
                "0mlrlskwwhirxk3wsz9psh6nxcy491n0dh8zl02qgj0jzpssw7i4"))

(define rust-lock-api-0.4.14
  (crate-source "lock_api" "0.4.14"
                "0rg9mhx7vdpajfxvdjmgmlyrn20ligzqvn8ifmaz7dc79gkrjhr2"))

(define rust-lock-api-0.5.0
  (crate-source "lock_api" "0.5.0"
                "1bsr9bn8b8fh1nq077l4yris6lizar4q7v6nykf175hpv8i1pnb5"))

(define rust-log-0.4.34
  (crate-source "log" "0.4.34"
                "1ihkzn0m33ab79fcl4mkb04n5iwqzbxzyw7l7hazqkffaqzbvy7r"))

(define rust-lsp-server-0.10.0
  (crate-source "lsp-server" "0.10.0"
                "0mknzz022b8w7flxh05yypicgbqc8mwn32gjxqkf8wg5y8qmmqiy"))

(define rust-manyhow-0.14.1
  (crate-source "manyhow" "0.14.1"
                "1iyqkq8bkrhyb5jv1472q8i91d3k3zwrj1dphmkjl9qn6c3lnw7i"))

(define rust-manyhow-macros-0.14.1
  (crate-source "manyhow-macros" "0.14.1"
                "099pjn6japlsiizblb2gxql2b9hk5l2s72m167wbcpqz3yvisq1i"))

(define rust-markdown-1.0.0
  (crate-source "markdown" "1.0.0"
                "1sqxbclkxw615kcwglcisda1dcw8cfaa30z7sa16lhfwrbrbijm5"))

(define rust-matchers-0.2.0
  (crate-source "matchers" "0.2.0"
                "1sasssspdj2vwcwmbq3ra18d3qniapkimfcbr47zmx6750m5llni"))

(define rust-matchit-0.9.2
  (crate-source "matchit" "0.9.2"
                "025nhwk0j1lfhnadxqkg4aqjqqdkqvmqq01n9s59l6qw023vaqw8"))

(define rust-memchr-2.8.3
  (crate-source "memchr" "2.8.3"
                "161xa63ipfanf8v3nb82xd5hqgydv55nzw59wyngqbz6alfaz2yg"))

(define rust-memoffset-0.9.1
  (crate-source "memoffset" "0.9.1"
                "12i17wh9a9plx869g7j4whf62xw68k5zd4k0k5nh6ys5mszid028"))

(define rust-mimalloc-0.1.52
  (crate-source "mimalloc" "0.1.52"
                "0qkqr4yga7fkyqwnn89d2xp346q54n4fpm91rzxd2jni52xkjh9d"))

(define rust-minicov-0.3.8
  (crate-source "minicov" "0.3.8"
                "0kg2bajhhzwafpvhqpvfaddrwy6z0ggvqlirdpb0b5jnj6jbcsa8"))

(define rust-minimal-lexical-0.2.1
  (crate-source "minimal-lexical" "0.2.1"
                "16ppc5g84aijpri4jzv14rvcnslvlpphbszc7zzp6vfkddf4qdb8"))

(define rust-miniz-oxide-0.8.9
  (crate-source "miniz_oxide" "0.8.9"
                "05k3pdg8bjjzayq3rf0qhpirq9k37pxnasfn4arbs17phqn6m9qz"))

(define rust-mio-1.2.4
  (crate-source "mio" "1.2.4"
                "1vl6px9zy0pwhlwnw3cknsqyzkg8bcpiwiq4cgicf2fwgywfv20p"))

(define rust-munge-0.4.7
  (crate-source "munge" "0.4.7"
                "032sj47l2174dirkjkhi18x92wlgdqdld4b4l5n9bfly4lgl05sy"))

(define rust-munge-macro-0.4.7
  (crate-source "munge_macro" "0.4.7"
                "0cgrm4q8a6qm0802d08pacbv2mpcq4c47hrxc3avannlrdfg4s25"))

(define rust-natord-1.0.9
  (crate-source "natord" "1.0.9"
                "0z75spwag3ch20841pvfwhh3892i2z2sli4pzp1jgizbipdrd39h"))

(define rust-nix-0.31.3
  (crate-source "nix" "0.31.3"
                "0gbwnjfny9rq9hl5bz4ry520n9rnfknna4bg88n66f7zx3yx486g"))

(define rust-nom-7.1.3
  (crate-source "nom" "7.1.3"
                "0jha9901wxam390jcf5pfa0qqfrgh8li787jx2ip0yk5b8y9hwyj"))

(define rust-normalize-line-endings-0.3.0
  (crate-source "normalize-line-endings" "0.3.0"
                "1gp52dfn2glz26a352zra8h04351icf0fkqzw1shkwrgh1vpz031"))

(define rust-notify-8.2.0
  (crate-source "notify" "8.2.0"
                "1hrb83451vm5cpjw83nz5skgwjg5ara28zq8nxsqbzsif690fgad"))

(define rust-notify-types-2.0.0
  (crate-source "notify-types" "2.0.0"
                "0pcjm3wnvb7pvzw6mn89csv64ip0xhx857kr8jic5vddi6ljc22y"))

(define rust-nu-ansi-term-0.50.1
  (crate-source "nu-ansi-term" "0.50.1"
                "16a3isvbxx8pa3lk71h3cq2fsx2d17zzq42j4mhpxy81gl2qx8nl"))

(define rust-num-traits-0.2.19
  (crate-source "num-traits" "0.2.19"
                "0h984rhdkkqd4ny9cif7y2azl3xdfb7768hb9irhpsch4q3gq787"))

(define rust-objc2-0.6.3
  (crate-source "objc2" "0.6.3"
                "01ccrb558qav2rqrmk0clzqzdd6r1rmicqnf55xqam7cw2f5khmp"))

(define rust-objc2-encode-4.1.0
  (crate-source "objc2-encode" "4.1.0"
                "0cqckp4cpf68mxyc2zgnazj8klv0z395nsgbafa61cjgsyyan9gg"))

(define rust-object-0.39.1
  (crate-source "object" "0.39.1"
                "16vkcaamik55jd9f04g73hvgsm5w636gb4w06x3nafvsih4nqnif"))

(define rust-once-cell-1.21.4
  (crate-source "once_cell" "1.21.4"
                "0l1v676wf71kjg2khch4dphwh1jp3291ffiymr2mvy1kxd5kwz4z"))

(define rust-once-cell-polyfill-1.70.1
  (crate-source "once_cell_polyfill" "1.70.1"
                "1bg0w99srq8h4mkl68l1mza2n2f2hvrg0n8vfa3izjr5nism32d4"))

(define rust-oorandom-11.1.5
  (crate-source "oorandom" "11.1.5"
                "07mlf13z453fq01qff38big1lh83j8l6aaglf63ksqzzqxc0yyfn"))

(define rust-option-ext-0.2.0
  (crate-source "option-ext" "0.2.0"
                "0zbf7cx8ib99frnlanpyikm1bx8qn8x602sw1n7bg6p9x94lyx04"))

(define rust-ordermap-1.2.2
  (crate-source "ordermap" "1.2.2"
                "188wy0253gk7lir08dds6md8319827nz1g97i7y0wv7brpfmd9qn"))

(define rust-os-pipe-1.2.2
  (crate-source "os_pipe" "1.2.2"
                "090jhg0i1pj6l9w9zm0p0r7lv89kpwkj8vqij1iaskmic13mycyv"))

(define rust-os-str-bytes-7.1.1
  (crate-source "os_str_bytes" "7.1.1"
                "0p5dqfb1w2p33h85vxl1qaq7zn0mvcij3sq8gnl12w3mbmxypv33"))

(define rust-page-size-0.6.0
  (crate-source "page_size" "0.6.0"
                "1nj0rrwpvagagssljbm29ww1iyrrg15p1q4sk70r2cfi9qcv5m9h"))

(define rust-parking-lot-0.12.5
  (crate-source "parking_lot" "0.12.5"
                "06jsqh9aqmc94j2rlm8gpccilqm6bskbd67zf6ypfc0f4m9p91ck"))

(define rust-parking-lot-0.13.0
  (crate-source "parking_lot" "0.13.0"
                "09njdx4da5163x3h43x0qpkrmhrg89ir0mc4hxbvgl71liy97kx9"))

(define rust-parking-lot-core-0.9.12
  (crate-source "parking_lot_core" "0.9.12"
                "1hb4rggy70fwa1w9nb0svbyflzdc69h047482v2z3sx2hmcnh896"))

(define rust-parking-lot-core-0.10.0
  (crate-source "parking_lot_core" "0.10.0"
                "09lqs4hjq55zz70gbiszx2wpz366h2pzq42hh6kqmhcc0rhg1zww"))

(define rust-paste-1.0.15
  (crate-source "paste" "1.0.15"
                "02pxffpdqkapy292harq6asfjvadgp1s005fip9ljfsn9fvxgh2p"))

(define rust-path-absolutize-4.0.1
  (crate-source "path-absolutize" "4.0.1"
                "1bb802xiv5ab7csv061wfjhhj40x9g8qv8k7kx306ivrfllp827q"))

(define rust-path-dedot-4.0.1
  (crate-source "path-dedot" "4.0.1"
                "19n5r9hcg3m5k9wqv7gg0gy81w01w6iwd3a02m012v063h7isd83"))

(define rust-path-slash-0.2.1
  (crate-source "path-slash" "0.2.1"
                "0hjgljv4vy97qqw9gxnwzqhhpysjss2yhdphfccy3c388afhk48y"))

(define rust-pathdiff-0.2.3
  (crate-source "pathdiff" "0.2.3"
                "1lrqp4ip05df8dzldq6gb2c1sq2gs54gly8lcnv3rhav1qhwx56z"))

(define rust-peg-0.8.5
  (crate-source "peg" "0.8.5"
                "0xr42lbgihlwbyfcmjggqv1s3a38agp4j077cd8yqdhv235cya4r"))

(define rust-peg-macros-0.8.5
  (crate-source "peg-macros" "0.8.5"
                "0w8g53c75hz6iy3khc1ja2qm0idpnygjc0xsa9fmpyh2q82ap632"))

(define rust-peg-runtime-0.8.5
  (crate-source "peg-runtime" "0.8.5"
                "1jkgfy5jkjqwqy7h11nn3bmm1qfyn9kq2wnxnlspp4ldhsdwlb8k"))

(define rust-pep440-rs-0.7.3
  (crate-source "pep440_rs" "0.7.3"
                "177vv3fvdsp80x9hi2wigw3hkg7pxq6v4hjzfhrdxqwnyfhmq29i"))

(define rust-pep508-rs-0.9.2
  (crate-source "pep508_rs" "0.9.2"
                "01fcbf9vq8ya3shlsmx04fyz5n7h4vm8ixrgrnnzq8a10qkp5vps"))

(define rust-percent-encoding-2.3.2
  (crate-source "percent-encoding" "2.3.2"
                "083jv1ai930azvawz2khv7w73xh8mnylk7i578cifndjn5y64kwv"))

(define rust-pest-2.8.2
  (crate-source "pest" "2.8.2"
                "1a6g94pr4npsg0s6ljddwp4g127ks0nygzhxcpwfmyik6yis7q11"))

(define rust-pest-derive-2.8.2
  (crate-source "pest_derive" "2.8.2"
                "0qy6nv84q1m6m2rzw1qjfbxlcizz7h557rkk16yivjqafxpp0n5w"))

(define rust-pest-generator-2.8.2
  (crate-source "pest_generator" "2.8.2"
                "0bws5i6g3v1sldvy66p7qbzmz5mqbiflcqilaywgf1zy3n0kckvd"))

(define rust-pest-meta-2.8.2
  (crate-source "pest_meta" "2.8.2"
                "0844iv71ibf414yid0bvhi9i0zfi0jrmyh6mvjjx1jws102rp4a2"))

(define rust-phf-0.11.3
  (crate-source "phf" "0.11.3"
                "0y6hxp1d48rx2434wgi5g8j1pr8s5jja29ha2b65435fh057imhz"))

(define rust-phf-0.13.1
  (crate-source "phf" "0.13.1"
                "1pzswx5gdglgjgp4azyzwyr4gh031r0kcnpqq6jblga72z3jsmn1"))

(define rust-phf-0.14.0
  (crate-source "phf" "0.14.0"
                "1xm2nbg5c59b5wvvv4v3sd3sknrlg2z17bkzk440p2090dw7h0q1"))

(define rust-phf-codegen-0.11.3
  (crate-source "phf_codegen" "0.11.3"
                "0si1n6zr93kzjs3wah04ikw8z6npsr39jw4dam8yi9czg2609y5f"))

(define rust-phf-codegen-0.14.0
  (crate-source "phf_codegen" "0.14.0"
                "1zkgzi52lx4r968cmg3ciqfs40raxy17k2fnxbmzsxpv22jqbda1"))

(define rust-phf-generator-0.11.3
  (crate-source "phf_generator" "0.11.3"
                "0gc4np7s91ynrgw73s2i7iakhb4lzdv1gcyx7yhlc0n214a2701w"))

(define rust-phf-generator-0.14.0
  (crate-source "phf_generator" "0.14.0"
                "003i8qw6ghkcvs6bmxdhnq1a1kmpwbciakazjsybx8fmb44jxdmf"))

(define rust-phf-shared-0.11.3
  (crate-source "phf_shared" "0.11.3"
                "1rallyvh28jqd9i916gk5gk2igdmzlgvv5q0l3xbf3m6y8pbrsk7"))

(define rust-phf-shared-0.13.1
  (crate-source "phf_shared" "0.13.1"
                "0rpjchnswm0x5l4mz9xqfpw0j4w68sjvyqrdrv13h7lqqmmyyzz5"))

(define rust-phf-shared-0.14.0
  (crate-source "phf_shared" "0.14.0"
                "12dalcsaq83pfydhq81195aal0ld9vdx3swz6kk9ncfrw8kr1zf6"))

(define rust-pin-project-lite-0.2.16
  (crate-source "pin-project-lite" "0.2.16"
                "16wzc7z7dfkf9bmjin22f5282783f6mdksnr0nv0j5ym5f9gyg1v"))

(define rust-pkg-config-0.3.32
  (crate-source "pkg-config" "0.3.32"
                "0k4h3gnzs94sjb2ix6jyksacs52cf1fanpwsmlhjnwrdnp8dppby"))

(define rust-portable-atomic-1.13.1
  (crate-source "portable-atomic" "1.13.1"
                "0j8vlar3n5acyigq8q6f4wjx3k3s5yz0rlpqrv76j73gi5qr8fn3"))

(define rust-portable-atomic-util-0.2.4
  (crate-source "portable-atomic-util" "0.2.4"
                "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))

(define rust-potential-utf-0.1.3
  (crate-source "potential_utf" "0.1.3"
                "12mhwvhpvvim6xqp6ifgkh1sniv9j2cmid6axn10fnjvpsnikpw4"))

(define rust-predicates-3.1.4
  (crate-source "predicates" "3.1.4"
                "1ziwwshyl5d7yf9anyb8ldamqrx0kv1w3mhdnzkpx8i85y9z5a5d"))

(define rust-predicates-core-1.0.10
  (crate-source "predicates-core" "1.0.10"
                "0i6ia05imr1fsppc1z2lg0g2kpalz7crmlx0n4ql0sqnyd38glya"))

(define rust-predicates-tree-1.0.13
  (crate-source "predicates-tree" "1.0.13"
                "1wp2farzvl4aarpa3sdq59bd1rk0zzqrszj6n0fi7j1rgf21ppnh"))

(define rust-pretty-assertions-1.4.1
  (crate-source "pretty_assertions" "1.4.1"
                "0v8iq35ca4rw3rza5is3wjxwsf88303ivys07anc5yviybi31q9s"))

(define rust-prettyplease-0.2.37
  (crate-source "prettyplease" "0.2.37"
                "0azn11i1kh0byabhsgab6kqs74zyrg69xkirzgqyhz6xmjnsi727"))

(define rust-proc-macro-crate-3.5.0
  (crate-source "proc-macro-crate" "3.5.0"
                "0kv1g1d1zjwxlgcaba2qlshzyy32j03xic8rskqlcr5mnblsfyz6"))

(define rust-proc-macro-error-attr2-2.0.0
  (crate-source "proc-macro-error-attr2" "2.0.0"
                "1ifzi763l7swl258d8ar4wbpxj4c9c2im7zy89avm6xv6vgl5pln"))

(define rust-proc-macro-error2-2.0.1
  (crate-source "proc-macro-error2" "2.0.1"
                "00lq21vgh7mvyx51nwxwf822w2fpww1x0z8z0q47p8705g2hbv0i"))

(define rust-proc-macro-utils-0.10.0
  (crate-source "proc-macro-utils" "0.10.0"
                "0wbhpdb1rp4h6f6m82ignhpb2hgji30bvdbpb0hvq0747nhhibzf"))

(define rust-proc-macro2-1.0.107
  (crate-source "proc-macro2" "1.0.107"
                "1nb6ly8kp65f724kj73ippc7lvydss24sm2vagk6qpklpg4pwplq"))

(define rust-psm-0.1.32
  (crate-source "psm" "0.1.32"
                "08m67yndaikwq8kviipphi50lfb25ph7j3gp4w3rffz6k52h7kad"))

(define rust-ptr-meta-0.3.1
  (crate-source "ptr_meta" "0.3.1"
                "0yaa4bvghj0rygqjlcd4lkcid58pcywxmjzisihsz5hibbwhr6hb"))

(define rust-ptr-meta-derive-0.3.1
  (crate-source "ptr_meta_derive" "0.3.1"
                "1qbg3malg24dmiszfddd7n69g3rb7vl7nxj67gchh4ky19yqcivk"))

(define rust-pyproject-toml-0.13.7
  (crate-source "pyproject-toml" "0.13.7"
                "1ljzdbims6p1acin25rzyysaaqal4f2j5d8kczkljjyi7945bmzn"))

(define rust-quick-xml-0.41.0
  (crate-source "quick-xml" "0.41.0"
                "1h9y8zry34r3mxfd5vqfj50vvvzvri4kzbx5d657jkqjalg4aq76"))

(define rust-quickcheck-1.1.0
  (crate-source "quickcheck" "1.1.0"
                "02zpl1i6xkfr2kw09j3h2ig0z4n63xxx4z4a2sm6l3yv6prqkicm"))

(define rust-quickcheck-macros-1.2.0
  (crate-source "quickcheck_macros" "1.2.0"
                "192vdkh1z3kk6srbyadqavagfcyrhbd49n8x2y5lqrnxjf28p8m9"))

(define rust-quote-1.0.47
  (crate-source "quote" "1.0.47"
                "00ch0yyzvv6s671ik0kcsbw8nigdaj2g3fr61kcahwx48aqlvgqz"))

(define rust-quote-use-0.9.0
  (crate-source "quote-use" "0.9.0"
                "0g9zns7bi4a02g28ywfxzf5c9kjf43zv7iwzgqfkrp13714nwda8"))

(define rust-quote-use-macros-0.9.0
  (crate-source "quote-use-macros" "0.9.0"
                "01078fl98a2dnmb920m7ks2rg401qyj7miq33v3k3hkwdbgcbwcc"))

(define rust-r-efi-5.3.0
  (crate-source "r-efi" "5.3.0"
                "03sbfm3g7myvzyylff6qaxk4z6fy76yv860yy66jiswc2m6b7kb9"))

(define rust-r-efi-6.0.0
  (crate-source "r-efi" "6.0.0"
                "1gyrl2k5fyzj9k7kchg2n296z5881lg7070msabid09asp3wkp7q"))

(define rust-radium-0.7.0
  (crate-source "radium" "0.7.0"
                "02cxfi3ky3c4yhyqx9axqwhyaca804ws46nn4gc1imbk94nzycyw"))

(define rust-rancor-0.1.1
  (crate-source "rancor" "0.1.1"
                "1vl1y0yhw40j3g6b2h9jgkfjp0pg000cia8xashc49qm71rflqx0"))

(define rust-rand-0.8.8
  (crate-source "rand" "0.8.8"
                "0k3d9psya5icpiylff1w1wjbnc3q4pb1953n1iw7gbr61ggcfn70"))

(define rust-rand-0.10.3
  (crate-source "rand" "0.10.3"
                "1bxlhj4m9zrgfgk1yirf0nny86izrngscydfx9w387n9rfbgpjb5"))

(define rust-rand-core-0.6.4
  (crate-source "rand_core" "0.6.4"
                "0b4j2v4cb5krak1pv6kakv4sz6xcwbrmy2zckc32hsigbrwy82zc"))

(define rust-rand-core-0.10.0
  (crate-source "rand_core" "0.10.0"
                "1flazfw1q1hbvadwzmaliplz0xnnjijdnbmzxnzdqplhfzb0z38c"))

(define rust-rayon-1.12.0
  (crate-source "rayon" "1.12.0"
                "0vcj63xgnk72c30vdrak7dhl53snnaqv9x2faf1d94hzg1kb2fgv"))

(define rust-rayon-core-1.13.0
  (crate-source "rayon-core" "1.13.0"
                "14dbr0sq83a6lf1rfjq5xdpk5r6zgzvmzs5j6110vlv2007qpq92"))

(define rust-redox-syscall-0.5.17
  (crate-source "redox_syscall" "0.5.17"
                "0xrvpchkaxph3r5ww2i04v9nwg3843fp3prf8kqlh1gv01b4c1sl"))

(define rust-redox-syscall-0.9.5
  (crate-source "redox_syscall" "0.9.5"
                "1zb8g34sfgrbvja7lgay2isggkj461qf86mdchawn7ml8mc8f32q"))

(define rust-redox-users-0.5.2
  (crate-source "redox_users" "0.5.2"
                "1b17q7gf7w8b1vvl53bxna24xl983yn7bd00gfbii74bcg30irm4"))

(define rust-ref-cast-1.0.25
  (crate-source "ref-cast" "1.0.25"
                "0zdzc34qjva9xxgs889z5iz787g81hznk12zbk4g2xkgwq530m7k"))

(define rust-ref-cast-impl-1.0.25
  (crate-source "ref-cast-impl" "1.0.25"
                "1nkhn1fklmn342z5c4mzfzlxddv3x8yhxwwk02cj06djvh36065p"))

(define rust-regex-1.13.1
  (crate-source "regex" "1.13.1"
                "1391a0a4100ik8cp7l577p3ip3haqq03rd9c5vdr7vcfdixj687h"))

(define rust-regex-automata-0.4.18
  (crate-source "regex-automata" "0.4.18"
                "1cml0rm0ssqfkibh9nh3gy4b6hbsbicj1rihpwf2a4v4nawm71dd"))

(define rust-regex-lite-0.1.7
  (crate-source "regex-lite" "0.1.7"
                "0c5s0kyc4gch0l0rzhm54prgfs169l2zwfvnzn91rvv33hr42gwl"))

(define rust-regex-syntax-0.8.11
  (crate-source "regex-syntax" "0.8.11"
                "1m25h5q2wp976fb9gc3dsc9l99svcvd5cri8lncb51c46ydgzxnn"))

(define rust-rend-0.5.3
  (crate-source "rend" "0.5.3"
                "1rknl9l1s3x67zizrxz1n3k8w5z7z54dqzsdlrahgwn22zrxxnna"))

(define rust-rkyv-0.8.18
  (crate-source "rkyv" "0.8.18"
                "16fkq8jyzckzjm10cv6wa5yab5vxgd7ra1hlmda085yany9n0xyr"))

(define rust-rkyv-derive-0.8.18
  (crate-source "rkyv_derive" "0.8.18"
                "0vmhkfnz6b7dn1qzckqjdy39fg525salhinn8jfq7pf799hfy98w"))

(define rust-ron-0.12.0
  (crate-source "ron" "0.12.0"
                "0cpc47zxmpmz01wxf70npyp3gdn0p66d5js98kqr661631dhqjgx"))

(define rust-rust-stemmers-1.2.0
  (crate-source "rust-stemmers" "1.2.0"
                "0m6acgdflrrcm17dj7lp7x4sfqqhga24qynv660qinwz04v20sp4"))

(define rust-rustc-hash-2.1.3
  (crate-source "rustc-hash" "2.1.3"
                "0bbla578m87qmf3yr55q49l97gxn7z0ha1dwqlnvwwc58ad7y7kb"))

(define rust-rustc-stable-hash-0.1.2
  (crate-source "rustc-stable-hash" "0.1.2"
                "026drx2ly2b8b1pp1c2v3p3ws6k0jaa5bbc5f4xwkibhj7r4453q"))

(define rust-rustix-1.1.5
  (crate-source "rustix" "1.1.5"
                "17b2srw7rcqmrs1shj89g8i3r1447lihv7qrbxvp11j1psxgl7l9"))

(define rust-rustversion-1.0.22
  (crate-source "rustversion" "1.0.22"
                "0vfl70jhv72scd9rfqgr2n11m5i9l1acnk684m2w83w0zbqdx75k"))

(define rust-ryu-1.0.20
  (crate-source "ryu" "1.0.20"
                "07s855l8sb333h6bpn24pka5sp7hjk2w667xy6a0khkf6sqv5lr8"))

(define rust-salsa-0.28.6
  (crate-source "salsa" "0.28.6"
                "0csmr3lp5bdi5xgyz0kh7yif5xmm3sz9q9yf24nm2mmlxqi63fvi"))

(define rust-salsa-macro-rules-0.28.6
  (crate-source "salsa-macro-rules" "0.28.6"
                "0sarn699pazc7i2hrhx745v9l83ldxccxxz7c6zmv10116zzgi8v"))

(define rust-salsa-macros-0.28.6
  (crate-source "salsa-macros" "0.28.6"
                "16ysd994gvriqqz27nspzgkilcd9awcrdh5szapb16ba7mhcsvbd"))

(define rust-same-file-1.0.6
  (crate-source "same-file" "1.0.6"
                "00h5j1w87dmhnvbv9l8bic3y7xxsnjmssvifw2ayvgx9mb1ivz4k"))

(define rust-schemars-1.2.3
  (crate-source "schemars" "1.2.3"
                "0vnzz8smh3ixzy297898mwzfx0r5aadz2vxsbafwlxsaywk8hl5b"))

(define rust-schemars-derive-1.2.3
  (crate-source "schemars_derive" "1.2.3"
                "0sgwpzi91y6ibw7ns4wpqzgm9ddnmi7q8ia62am62zgsjhhakhz1"))

(define rust-scopeguard-1.2.0
  (crate-source "scopeguard" "1.2.0"
                "0jcz9sd47zlsgcnm1hdw0664krxwb5gczlif4qngj2aif8vky54l"))

(define rust-seahash-4.1.0
  (crate-source "seahash" "4.1.0"
                "0sxsb64np6bvnppjz5hg4rqpnkczhsl8w8kf2a5lr1c08xppn40w"))

(define rust-semver-1.0.27
  (crate-source "semver" "1.0.27"
                "1qmi3akfrnqc2hfkdgcxhld5bv961wbk8my3ascv5068mc5fnryp"))

(define rust-serde-1.0.229
  (crate-source "serde" "1.0.229"
                "1fp04fq4a79bpm61xz1zy0pbz4kpc7d771zii1k3inmszq55jj21"))

(define rust-serde-wasm-bindgen-0.6.5
  (crate-source "serde-wasm-bindgen" "0.6.5"
                "0sz1l4v8059hiizf5z7r2spm6ws6sqcrs4qgqwww3p7dy1ly20l3"))

(define rust-serde-core-1.0.229
  (crate-source "serde_core" "1.0.229"
                "0j1ajiha76h3nmd976il9li6975k121xa7jb39ws8n0yqp4s5p37"))

(define rust-serde-derive-1.0.229
  (crate-source "serde_derive" "1.0.229"
                "0j4k63i7h1bikxwz2c89ig0hrwbnl9mz1czn85xx99x5cc9dg9g7"))

(define rust-serde-derive-internals-0.30.0
  (crate-source "serde_derive_internals" "0.30.0"
                "1gd9n45na7n79nr54ghfl79rygkbzw2ybk3wyr6nlp83rry16lpq"))

(define rust-serde-json-1.0.154
  (crate-source "serde_json" "1.0.154"
                "1mjby21g8fxhrsfp73yz8wg6n2f4h23ah0nczds409l53f5wrsg7"))

(define rust-serde-spanned-1.1.2
  (crate-source "serde_spanned" "1.1.2"
                "0b0crd16vvp0n81pgh0ldi3apld0ph5ywsrml80y5v2ynnz26xaf"))

(define rust-serde-test-1.0.177
  (crate-source "serde_test" "1.0.177"
                "1vgisk4dgwdmz4prc2iwq8lddrp4pbqdbljk0h3b7dnafgjix43z"))

(define rust-sha2-0.10.9
  (crate-source "sha2" "0.10.9"
                "10xjj843v31ghsksd9sl9y12qfc48157j1xpb8v1ml39jy0psl57"))

(define rust-sharded-slab-0.1.7
  (crate-source "sharded-slab" "0.1.7"
                "1xipjr4nqsgw34k7a2cgj9zaasl2ds6jwn89886kww93d32a637l"))

(define rust-shellexpand-3.1.2
  (crate-source "shellexpand" "3.1.2"
                "1n3y55yvh2s8cqmqb6bnz4wrlhchjd489fn1dpcc9rhnbsmlz0ij"))

(define rust-shlex-2.0.1
  (crate-source "shlex" "2.0.1"
                "1fjsll1cd7d2bcpdij9kd6w62rpbc7qqzvydvs021vsmr1cxvypq"))

(define rust-simdutf8-0.1.5
  (crate-source "simdutf8" "0.1.5"
                "0vmpf7xaa0dnaikib5jlx6y4dxd3hxqz6l830qb079g7wcsgxag3"))

(define rust-similar-2.7.0
  (crate-source "similar" "2.7.0"
                "1aidids7ymfr96s70232s6962v5g9l4zwhkvcjp4c5hlb6b5vfxv"))

(define rust-similar-3.2.0
  (crate-source "similar" "3.2.0"
                "11q997dzvhiq06wivkalbxrrgj7zzqifnhn923f7896ag8gwlrjg"))

(define rust-siphasher-1.0.1
  (crate-source "siphasher" "1.0.1"
                "17f35782ma3fn6sh21c027kjmd227xyrx06ffi8gw4xzv9yry6an"))

(define rust-slab-0.4.12
  (crate-source "slab" "0.4.12"
                "1xcwik6s6zbd3lf51kkrcicdq2j4c1fw0yjdai2apy9467i0sy8c"))

(define rust-smallvec-1.16.3
  (crate-source "smallvec" "1.16.3"
                "06z0akmcq2zc9qg22dxda298p50gg3di5ggl3y2nllag8ypwhgav"))

(define rust-snapbox-1.0.0
  (crate-source "snapbox" "1.0.0"
                "1sl9vxjkbxngy7s5b3wg09ssybdxqjzpmxq8hzlcnm40nrqhmmvi"))

(define rust-snapbox-macros-1.0.0
  (crate-source "snapbox-macros" "1.0.0"
                "1ygxy7mxypgigfajmbd9ks27s6rmhmlkgh29f4psnmhl5vscwj6j"))

(define rust-stable-deref-trait-1.2.0
  (crate-source "stable_deref_trait" "1.2.0"
                "1lxjr8q2n534b2lhkxd6l6wcddzjvnksi58zv11f9y0jjmr15wd8"))

(define rust-stacker-0.1.25
  (crate-source "stacker" "0.1.25"
                "0rwrws4iyh8cay7pghzf2dy55pprwfn1vm805f5czfh6cza4jzvh"))

(define rust-static-assertions-1.1.0
  (crate-source "static_assertions" "1.1.0"
                "0gsl6xmw10gvn3zs1rv99laj5ig7ylffnh71f9l34js4nr4r7sx2"))

(define rust-statrs-0.18.0
  (crate-source "statrs" "0.18.0"
                "0pikgp74gg9a3jp2hhh5z6wdfjn96gdkahw7n1kff4k5ik1ffgra"))

(define rust-strip-ansi-escapes-0.2.1
  (crate-source "strip-ansi-escapes" "0.2.1"
                "0980min1s9f5g47rwlq8l9njks952a0jlz0v7yxrm5p7www813ra"))

(define rust-strsim-0.11.1
  (crate-source "strsim" "0.11.1"
                "0kzvqlw8hxqb7y598w1s0hxlnmi84sg5vsipp3yg5na5d1rvba3x"))

(define rust-strum-0.28.0
  (crate-source "strum" "0.28.0"
                "1ggr0if083c1mz9w33hkdjsp0iqk2fz9n49bvb73knwihydxwa4n"))

(define rust-strum-macros-0.28.0
  (crate-source "strum_macros" "0.28.0"
                "0r7n6v5b3x85m52isyc8wq78irmr22g0hmj1xn3pbq8f4yhfx1db"))

(define rust-supports-hyperlinks-3.2.0
  (crate-source "supports-hyperlinks" "3.2.0"
                "14byz5m3mcfz8jcg3vd639sp5qvd6svs05di40qvik0i7d9bd5p3"))

(define rust-syn-2.0.119
  (crate-source "syn" "2.0.119"
                "15vjy620l91a3q4n4f4gzhnflmdr6pnm38v2m6cpk86i8av32a47"))

(define rust-syn-3.0.9
  (crate-source "syn" "3.0.9"
                "0fw28lhl90kls24q2h2sp39yjb0lsvz5cwh9fd3f3w3v9kp8v36p"))

(define rust-synstructure-0.13.2
  (crate-source "synstructure" "0.13.2"
                "1lh9lx3r3jb18f8sbj29am5hm9jymvbwh6jb1izsnnxgvgrp12kj"))

(define rust-tap-1.0.1
  (crate-source "tap" "1.0.1"
                "0sc3gl4nldqpvyhqi3bbd0l9k7fngrcl4zs47n314nqqk4bpx4sm"))

(define rust-tempfile-3.27.0
  (crate-source "tempfile" "3.27.0"
                "1gblhnyfjsbg9wjg194n89wrzah7jy3yzgnyzhp56f3v9jd7wj9j"))

(define rust-terminal-size-0.4.4
  (crate-source "terminal_size" "0.4.4"
                "0x4839vhhpzacc42rqj2wjhivlhlggzz3890b0c5pmyb3j11n2i3"))

(define rust-terminfo-0.9.0
  (crate-source "terminfo" "0.9.0"
                "0qp6rrzkxcg08vjzsim2bw7mid3vi29mizrg70dzbycj0q7q3snl"))

(define rust-termtree-0.5.1
  (crate-source "termtree" "0.5.1"
                "10s610ax6nb70yi7xfmwcb6d3wi9sj5isd0m63gy2pizr2zgwl4g"))

(define rust-test-case-3.4.0
  (crate-source "test-case" "3.4.0"
                "0hh0zd3bcryc6dwpcq71w9lgnr2smvj9ks47zgyb5c3wyvkm6j8j"))

(define rust-test-case-core-3.4.0
  (crate-source "test-case-core" "3.4.0"
                "1s53lpqm1l7yrprz0k2mncx9555mdd139fj64087w907nhapc2dx"))

(define rust-test-case-macros-3.4.0
  (crate-source "test-case-macros" "3.4.0"
                "10ih6c413y8p9l9vlj4syr9sj9rq2cwxdpp1cv0wmg27x7n3bkpl"))

(define rust-thin-vec-0.2.21
  (crate-source "thin-vec" "0.2.21"
                "0gm6kc1z58h5gaw2ad5pbvkpbm6gprkf4dwx757lmjrqhyxbk96n"))

(define rust-thiserror-1.0.69
  (crate-source "thiserror" "1.0.69"
                "0lizjay08agcr5hs9yfzzj6axs53a2rgx070a1dsi3jpkcrzbamn"))

(define rust-thiserror-2.0.21
  (crate-source "thiserror" "2.0.21"
                "17hq1lh5dyr3bkc7zzjrbrp4qgkvhc48kgq1n5fdxkindaw2rr89"))

(define rust-thiserror-impl-1.0.69
  (crate-source "thiserror-impl" "1.0.69"
                "1h84fmn2nai41cxbhk6pqf46bxqq1b344v8yz089w1chzi76rvjg"))

(define rust-thiserror-impl-2.0.21
  (crate-source "thiserror-impl" "2.0.21"
                "0945n8agp7kg6n6b35yyjb4g5xv2q22vrw15h6jj1nw76a99flgy"))

(define rust-thread-local-1.1.9
  (crate-source "thread_local" "1.1.9"
                "1191jvl8d63agnq06pcnarivf63qzgpws5xa33hgc92gjjj4c0pn"))

(define rust-tikv-jemalloc-sys-0.6.1+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "tikv-jemalloc-sys"
                "0.6.1+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7"
                "0frahmfl05hppiy1sz9g14qb5jv2q4wr323d83xcx8l6mfrab2nd"))

(define rust-tikv-jemallocator-0.6.1
  (crate-source "tikv-jemallocator" "0.6.1"
                "0fh55m13396lii5wj69qh547fq9n2k7r3cczwrkhaklmgwrb8n83"))

(define rust-tinystr-0.8.4
  (crate-source "tinystr" "0.8.4"
                "0hzncw8rgk4syla79qscfml46jm7ll1zdp7kdacc42cj8n8prqmi"))

(define rust-tinytemplate-1.2.1
  (crate-source "tinytemplate" "1.2.1"
                "1g5n77cqkdh9hy75zdb01adxn45mkh9y40wdr7l68xpz35gnnkdy"))

(define rust-tinyvec-1.10.0
  (crate-source "tinyvec" "1.10.0"
                "1yhk0qdqyiaa4v2j9h8pzax5gxgwpz4da0lcphfil6g6pk1zv9dz"))

(define rust-tinyvec-macros-0.1.1
  (crate-source "tinyvec_macros" "0.1.1"
                "081gag86208sc3y6sdkshgw3vysm5d34p431dzw0bshz66ncng0z"))

(define rust-toml-0.9.12+spec-1.1.0
  (crate-source "toml" "0.9.12+spec-1.1.0"
                "0qwqbrymqn88mg2yqyq3rj52z6p20448z0jxdbpjsbpwg5g894ng"))

(define rust-toml-1.1.8+spec-1.1.0
  (crate-source "toml" "1.1.8+spec-1.1.0"
                "13b7kqvw7aa4x08i03d2fpima1i75v0lqxl0wrmisx6qwh09wj10"))

(define rust-toml-datetime-0.7.5+spec-1.1.0
  (crate-source "toml_datetime" "0.7.5+spec-1.1.0"
                "0iqkgvgsxmszpai53dbip7sf2igic39s4dby29dbqf1h9bnwzqcj"))

(define rust-toml-datetime-1.1.2+spec-1.1.0
  (crate-source "toml_datetime" "1.1.2+spec-1.1.0"
                "0lrhcmqvhjr259w4f2kijya7fgi0kpmhg9fb3m144v3cj1kxg1ib"))

(define rust-toml-edit-0.25.12+spec-1.1.0
  (crate-source "toml_edit" "0.25.12+spec-1.1.0"
                "1mx5paq837rjw7w51zprrjynk1vaig9yzxfqz9ac79jmd7f3w5fj"))

(define rust-toml-parser-1.1.5+spec-1.1.0
  (crate-source "toml_parser" "1.1.5+spec-1.1.0"
                "0k3lljyi4zxchdklaqghkwbl7wkd2ab1w16hlyniqzid0fl979ms"))

(define rust-toml-writer-1.1.3+spec-1.1.0
  (crate-source "toml_writer" "1.1.3+spec-1.1.0"
                "0w17cps8mfg7wcwyp0gggi9cpgmxldb9pwl55qp8ssq5zj6bvg86"))

(define rust-tracing-0.1.44
  (crate-source "tracing" "0.1.44"
                "006ilqkg1lmfdh3xhg3z762izfwmxcvz0w7m4qx2qajbz9i1drv3"))

(define rust-tracing-attributes-0.1.31
  (crate-source "tracing-attributes" "0.1.31"
                "1np8d77shfvz0n7camx2bsf1qw0zg331lra0hxb4cdwnxjjwz43l"))

(define rust-tracing-core-0.1.36
  (crate-source "tracing-core" "0.1.36"
                "16mpbz6p8vd6j7sf925k9k8wzvm9vdfsjbynbmaxxyq6v7wwm5yv"))

(define rust-tracing-flame-0.2.0
  (crate-source "tracing-flame" "0.2.0"
                "1ad34bhy9gsj0ijn56jsvizydash6zcybbls29g1i2a7w5z13bhb"))

(define rust-tracing-indicatif-0.3.14
  (crate-source "tracing-indicatif" "0.3.14"
                "0zn4m71shnxa37hxvzfxnnh724b3js728wq513q4k1s3w286kvz1"))

(define rust-tracing-log-0.2.0
  (crate-source "tracing-log" "0.2.0"
                "1hs77z026k730ij1a9dhahzrl0s073gfa2hm5p0fbl0b80gmz1gf"))

(define rust-tracing-subscriber-0.3.23
  (crate-source "tracing-subscriber" "0.3.23"
                "06fkr0qhggvrs861d7f74pn3i3a10h5jsp4n70jj9ys5b675fzyb"))

(define rust-typed-arena-2.0.2
  (crate-source "typed-arena" "2.0.2"
                "0shj0jpmglhgw2f1i4b33ycdzwd1z205pbs1rd5wx7ks2qhaxxka"))

(define rust-typed-path-0.13.0
  (crate-source "typed-path" "0.13.0"
                "07jy8i3k23d02g9g30xyr5f0mbxijjb38brdcqlg1l4c7n19is4w"))

(define rust-typeid-1.0.3
  (crate-source "typeid" "1.0.3"
                "0727ypay2p6mlw72gz3yxkqayzdmjckw46sxqpaj08v0b0r64zdw"))

(define rust-typenum-1.18.0
  (crate-source "typenum" "1.18.0"
                "0gwgz8n91pv40gabrr1lzji0b0hsmg0817njpy397bq7rvizzk0x"))

(define rust-ucd-trie-0.1.7
  (crate-source "ucd-trie" "0.1.7"
                "0wc9p07sqwz320848i52nvyjvpsxkx3kv5bfbmm6s35809fdk5i8"))

(define rust-unicode-id-0.3.6
  (crate-source "unicode-id" "0.3.6"
                "1015prrd0dmy1p6zxymi7zjnnc5y6y6p2xp4rd1w09wrf272ifkh"))

(define rust-unicode-ident-1.0.24
  (crate-source "unicode-ident" "1.0.24"
                "0xfs8y1g7syl2iykji8zk5hgfi5jw819f5zsrbaxmlzwsly33r76"))

(define rust-unicode-normalization-0.1.25
  (crate-source "unicode-normalization" "0.1.25"
                "1s76dcrxw7vs32yhpi0p074apdc3s7lak7809f3qvclwij3zdm2z"))

(define rust-unicode-width-0.2.2
  (crate-source "unicode-width" "0.2.2"
                "0m7jjzlcccw716dy9423xxh0clys8pfpllc5smvfxrzdf66h9b5l"))

(define rust-unicode-xid-0.2.6
  (crate-source "unicode-xid" "0.2.6"
                "0lzqaky89fq0bcrh6jj6bhlz37scfd8c7dsj5dq7y32if56c1hgb"))

(define rust-unit-prefix-0.5.1
  (crate-source "unit-prefix" "0.5.1"
                "05rq0asf2f1q5vrcv4bwf0c3y6q20asqkiqpr8wqyrfxyb7h4d1j"))

(define rust-unscanny-0.1.0
  (crate-source "unscanny" "0.1.0"
                "0ivbipc1rnq15fhzgna41p1h01ncq4shycii72f3x5d7czq2mpz9"))

(define rust-url-2.5.8
  (crate-source "url" "2.5.8"
                "1v8f7nx3hpr1qh76if0a04sj08k86amsq4h8cvpw6wvk76jahrzz"))

(define rust-urlencoding-2.1.3
  (crate-source "urlencoding" "2.1.3"
                "1nj99jp37k47n0hvaz5fvz7z6jd0sb4ppvfy3nphr1zbnyixpy6s"))

(define rust-utf8-width-0.1.7
  (crate-source "utf8-width" "0.1.7"
                "1qwj8c0fg8cpn8hq7c9xzz26kdz6ci32bf0madz57a2xi578vgc6"))

(define rust-utf8-iter-1.0.4
  (crate-source "utf8_iter" "1.0.4"
                "1gmna9flnj8dbyd8ba17zigrp9c4c3zclngf5lnb5yvz1ri41hdn"))

(define rust-utf8parse-0.2.2
  (crate-source "utf8parse" "0.2.2"
                "088807qwjq46azicqwbhlmzwrbkz7l4hpw43sdkdyyk524vdxaq6"))

(define rust-uuid-1.28.0
  (crate-source "uuid" "1.28.0"
                "01mnlhz09hwdjaxwzjd6bql3fjb5zl9n8dzaxn7dvdxyhiiiihbw"))

(define rust-valuable-0.1.1
  (crate-source "valuable" "0.1.1"
                "0r9srp55v7g27s5bg7a2m095fzckrcdca5maih6dy9bay6fflwxs"))

(define rust-version-ranges-0.1.1
  (crate-source "version-ranges" "0.1.1"
                "0bgl8agnz3k3wsnydiq9qgahf4s0zvdbmbamqczyhazbbi0pkl7q"))

(define rust-version-check-0.9.5
  (crate-source "version_check" "0.9.5"
                "0nhhi4i5x89gm911azqbn7avs9mdacw2i3vcz3cnmz3mv4rqz4hb"))

(define rust-vt100-0.16.2
  (crate-source "vt100" "0.16.2"
                "1nbgsgamgibyx6y4xiyk6nkz7zggzbs6s445wq4yd0zsp1gzfkq5"))

(define rust-vte-0.14.1
  (crate-source "vte" "0.14.1"
                "0xy01fgkzb2080prh2ncd8949hm2248fc5wf1lryhdrhxzbxq7r3"))

(define rust-vte-0.15.0
  (crate-source "vte" "0.15.0"
                "1g9xgnw7q7zdwgfqa6zfcfsp92wn0j0h13kzsqy0dq3c80c414m5"))

(define rust-wait-timeout-0.2.1
  (crate-source "wait-timeout" "0.2.1"
                "04azqv9mnfxgvnc8j2wp362xraybakh2dy1nj22gj51rdl93pb09"))

(define rust-walkdir-2.5.0
  (crate-source "walkdir" "2.5.0"
                "0jsy7a710qv8gld5957ybrnc07gavppp963gs32xk4ag8130jy99"))

(define rust-wasi-0.11.1+wasi-snapshot-preview1
  (crate-source "wasi" "0.11.1+wasi-snapshot-preview1"
                "0jx49r7nbkbhyfrfyhz0bm4817yrnxgd3jiwwwfv0zl439jyrwyc"))

(define rust-wasip2-1.0.1+wasi-0.2.4
  (crate-source "wasip2" "1.0.1+wasi-0.2.4"
                "1rsqmpspwy0zja82xx7kbkbg9fv34a4a2if3sbd76dy64a244qh5"))

(define rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
  (crate-source "wasip3" "0.4.0+wasi-0.3.0-rc-2026-01-06"
                "19dc8p0y2mfrvgk3qw3c3240nfbylv22mvyxz84dqpgai2zzha2l"))

(define rust-wasm-bindgen-0.2.123
  (crate-source "wasm-bindgen" "0.2.123"
                "0qqmx07r597gm8lbz8qngvv0phwvpzzyfh3nl84nz9qr1jqs8m52"))

(define rust-wasm-bindgen-futures-0.4.73
  (crate-source "wasm-bindgen-futures" "0.4.73"
                "1bva12h8gdpqkp753czlxabs0s21lvgzm41brr4lhpdzz818fmjl"))

(define rust-wasm-bindgen-macro-0.2.123
  (crate-source "wasm-bindgen-macro" "0.2.123"
                "1p50xdwmv543b52bc49vm5lcsgd9adpx647bdisg7ihfbg3hz914"))

(define rust-wasm-bindgen-macro-support-0.2.123
  (crate-source "wasm-bindgen-macro-support" "0.2.123"
                "0nwqyc63byl7rp9nnv45av8h85fncfmxywkvy35d9qwwkfyk93wh"))

(define rust-wasm-bindgen-shared-0.2.123
  (crate-source "wasm-bindgen-shared" "0.2.123"
                "14lvjm3pzywm5c4962i6s5zmngic1knpggshnnxr9c97dihzgjvs"))

(define rust-wasm-bindgen-test-0.3.73
  (crate-source "wasm-bindgen-test" "0.3.73"
                "0ng1hpxf2a2zcyc61x2xjb2vf8f0qn3v9b1aysc2xkvkn610g2p2"))

(define rust-wasm-bindgen-test-macro-0.3.73
  (crate-source "wasm-bindgen-test-macro" "0.3.73"
                "0sx0l4div42mdns0gza7yplhy55l6kn3gkpp0gdavm8srr8pb6gf"))

(define rust-wasm-bindgen-test-shared-0.2.123
  (crate-source "wasm-bindgen-test-shared" "0.2.123"
                "1rfksr0iwb0r87gnia3829rij672ywfzg7m767zmwxhznrg003xf"))

(define rust-wasm-encoder-0.244.0
  (crate-source "wasm-encoder" "0.244.0"
                "06c35kv4h42vk3k51xjz1x6hn3mqwfswycmr6ziky033zvr6a04r"))

(define rust-wasm-metadata-0.244.0
  (crate-source "wasm-metadata" "0.244.0"
                "02f9dhlnryd2l7zf03whlxai5sv26x4spfibjdvc3g9gd8z3a3mv"))

(define rust-wasmparser-0.244.0
  (crate-source "wasmparser" "0.244.0"
                "1zi821hrlsxfhn39nqpmgzc0wk7ax3dv6vrs5cw6kb0v5v3hgf27"))

(define rust-web-sys-0.3.100
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "web-sys" "0.3.100"
                "0sffbkrpgyi1402mv4wzp9av6ky6rnb1d2m2dpf87wi7yfn7223f"))

(define rust-web-time-1.1.0
  (crate-source "web-time" "1.1.0"
                "1fx05yqx83dhx628wb70fyy10yjfq1jpl20qfqhdkymi13rq0ras"))

(define rust-which-8.0.6
  (crate-source "which" "8.0.6"
                "0kq3r101j662briq4hrlrd9jv090ppsr3k0smcf7lr0np2rg5qms"))

(define rust-wild-2.2.1
  (crate-source "wild" "2.2.1"
                "1q8hnhmv3fvgx0j7bv8qig00599a15mfsdhgx3hq2ljpiky1l4x3"))

(define rust-winapi-0.3.9
  (crate-source "winapi" "0.3.9"
                "06gl025x418lchw1wxj64ycr7gha83m44cjr5sarhynd9xkrm0sw"))

(define rust-winapi-i686-pc-windows-gnu-0.4.0
  (crate-source "winapi-i686-pc-windows-gnu" "0.4.0"
                "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))

(define rust-winapi-util-0.1.11
  (crate-source "winapi-util" "0.1.11"
                "08hdl7mkll7pz8whg869h58c1r9y7in0w0pk8fm24qc77k0b39y2"))

(define rust-winapi-x86-64-pc-windows-gnu-0.4.0
  (crate-source "winapi-x86_64-pc-windows-gnu" "0.4.0"
                "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))

(define rust-windows-link-0.1.3
  (crate-source "windows-link" "0.1.3"
                "12kr1p46dbhpijr4zbwr2spfgq8i8c5x55mvvfmyl96m01cx4sjy"))

(define rust-windows-link-0.2.1
  (crate-source "windows-link" "0.2.1"
                "1rag186yfr3xx7piv5rg8b6im2dwcf8zldiflvb22xbzwli5507h"))

(define rust-windows-sys-0.52.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.52.0"
                "0gd3v4ji88490zgb6b5mq5zgbvwv7zx1ibn8v3x83rwcdbryaar8"))

(define rust-windows-sys-0.59.0
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.59.0"
                "0fw5672ziw8b3zpmnbp9pdv1famk74f1l9fcbc3zsrzdg56vqf0y"))

(define rust-windows-sys-0.60.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.60.2"
                "1jrbc615ihqnhjhxplr2kw7rasrskv9wj3lr80hgfd42sbj01xgj"))

(define rust-windows-sys-0.61.2
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "windows-sys" "0.61.2"
                "1z7k3y9b6b5h52kid57lvmvm05362zv1v8w0gc7xyv5xphlp44xf"))

(define rust-windows-targets-0.52.6
  (crate-source "windows-targets" "0.52.6"
                "0wwrx625nwlfp7k93r2rra568gad1mwd888h1jwnl0vfg5r4ywlv"))

(define rust-windows-targets-0.53.3
  (crate-source "windows-targets" "0.53.3"
                "14fwwm136dhs3i1impqrrip7nvkra3bdxa4nqkblj604qhqn1znm"))

(define rust-windows-aarch64-gnullvm-0.52.6
  (crate-source "windows_aarch64_gnullvm" "0.52.6"
                "1lrcq38cr2arvmz19v32qaggvj8bh1640mdm9c2fr877h0hn591j"))

(define rust-windows-aarch64-gnullvm-0.53.0
  (crate-source "windows_aarch64_gnullvm" "0.53.0"
                "0r77pbpbcf8bq4yfwpz2hpq3vns8m0yacpvs2i5cn6fx1pwxbf46"))

(define rust-windows-aarch64-msvc-0.52.6
  (crate-source "windows_aarch64_msvc" "0.52.6"
                "0sfl0nysnz32yyfh773hpi49b1q700ah6y7sacmjbqjjn5xjmv09"))

(define rust-windows-aarch64-msvc-0.53.0
  (crate-source "windows_aarch64_msvc" "0.53.0"
                "0v766yqw51pzxxwp203yqy39ijgjamp54hhdbsyqq6x1c8gilrf7"))

(define rust-windows-i686-gnu-0.52.6
  (crate-source "windows_i686_gnu" "0.52.6"
                "02zspglbykh1jh9pi7gn8g1f97jh1rrccni9ivmrfbl0mgamm6wf"))

(define rust-windows-i686-gnu-0.53.0
  (crate-source "windows_i686_gnu" "0.53.0"
                "1hvjc8nv95sx5vdd79fivn8bpm7i517dqyf4yvsqgwrmkmjngp61"))

(define rust-windows-i686-gnullvm-0.52.6
  (crate-source "windows_i686_gnullvm" "0.52.6"
                "0rpdx1537mw6slcpqa0rm3qixmsb79nbhqy5fsm3q2q9ik9m5vhf"))

(define rust-windows-i686-gnullvm-0.53.0
  (crate-source "windows_i686_gnullvm" "0.53.0"
                "04df1in2k91qyf1wzizvh560bvyzq20yf68k8xa66vdzxnywrrlw"))

(define rust-windows-i686-msvc-0.52.6
  (crate-source "windows_i686_msvc" "0.52.6"
                "0rkcqmp4zzmfvrrrx01260q3xkpzi6fzi2x2pgdcdry50ny4h294"))

(define rust-windows-i686-msvc-0.53.0
  (crate-source "windows_i686_msvc" "0.53.0"
                "0pcvb25fkvqnp91z25qr5x61wyya12lx8p7nsa137cbb82ayw7sq"))

(define rust-windows-x86-64-gnu-0.52.6
  (crate-source "windows_x86_64_gnu" "0.52.6"
                "0y0sifqcb56a56mvn7xjgs8g43p33mfqkd8wj1yhrgxzma05qyhl"))

(define rust-windows-x86-64-gnu-0.53.0
  (crate-source "windows_x86_64_gnu" "0.53.0"
                "1flh84xkssn1n6m1riddipydcksp2pdl45vdf70jygx3ksnbam9f"))

(define rust-windows-x86-64-gnullvm-0.52.6
  (crate-source "windows_x86_64_gnullvm" "0.52.6"
                "03gda7zjx1qh8k9nnlgb7m3w3s1xkysg55hkd1wjch8pqhyv5m94"))

(define rust-windows-x86-64-gnullvm-0.53.0
  (crate-source "windows_x86_64_gnullvm" "0.53.0"
                "0mvc8119xpbi3q2m6mrjcdzl6afx4wffacp13v76g4jrs1fh6vha"))

(define rust-windows-x86-64-msvc-0.52.6
  (crate-source "windows_x86_64_msvc" "0.52.6"
                "1v7rb5cibyzx8vak29pdrk8nx9hycsjs4w0jgms08qk49jl6v7sq"))

(define rust-windows-x86-64-msvc-0.53.0
  (crate-source "windows_x86_64_msvc" "0.53.0"
                "11h4i28hq0zlnjcaqi2xdxr7ibnpa8djfggch9rki1zzb8qi8517"))

(define rust-winnow-0.7.13
  (crate-source "winnow" "0.7.13"
                "1krrjc1wj2vx0r57m9nwnlc1zrhga3fq41d8w9hysvvqb5mj7811"))

(define rust-winnow-1.0.0
  (crate-source "winnow" "1.0.0"
                "1n67gx8mg2b6r2z54zwbrb6qsfbdsar1lvafsfaajr3jcvj8h3m9"))

(define rust-wit-bindgen-0.46.0
  (crate-source "wit-bindgen" "0.46.0"
                "0ngysw50gp2wrrfxbwgp6dhw1g6sckknsn3wm7l00vaf7n48aypi"))

(define rust-wit-bindgen-0.51.0
  (crate-source "wit-bindgen" "0.51.0"
                "19fazgch8sq5cvjv3ynhhfh5d5x08jq2pkw8jfb05vbcyqcr496p"))

(define rust-wit-bindgen-core-0.51.0
  (crate-source "wit-bindgen-core" "0.51.0"
                "1p2jszqsqbx8k7y8nwvxg65wqzxjm048ba5phaq8r9iy9ildwqga"))

(define rust-wit-bindgen-rust-0.51.0
  (crate-source "wit-bindgen-rust" "0.51.0"
                "08bzn5fsvkb9x9wyvyx98qglknj2075xk1n7c5jxv15jykh6didp"))

(define rust-wit-bindgen-rust-macro-0.51.0
  (crate-source "wit-bindgen-rust-macro" "0.51.0"
                "0ymizapzv2id89igxsz2n587y2hlfypf6n8kyp68x976fzyrn3qc"))

(define rust-wit-component-0.244.0
  (crate-source "wit-component" "0.244.0"
                "1clwxgsgdns3zj2fqnrjcp8y5gazwfa1k0sy5cbk0fsmx4hflrlx"))

(define rust-wit-parser-0.244.0
  (crate-source "wit-parser" "0.244.0"
                "0dm7avvdxryxd5b02l0g5h6933z1cw5z0d4wynvq2cywq55srj7c"))

(define rust-writeable-0.6.4
  (crate-source "writeable" "0.6.4"
                "1p3r4s4wbf3dksfpj3xyrn7id5p0f7r74mj6qx6ngjfd6cm2vn1s"))

(define rust-wyz-0.5.1
  (crate-source "wyz" "0.5.1"
                "1vdrfy7i2bznnzjdl9vvrzljvs4s3qm8bnlgqwln6a941gy61wq5"))

(define rust-yansi-1.0.1
  (crate-source "yansi" "1.0.1"
                "0jdh55jyv0dpd38ij4qh60zglbw9aa8wafqai6m0wa7xaxk3mrfg"))

(define rust-yoke-0.8.3
  (crate-source "yoke" "0.8.3"
                "1xgyj6c2lxj2bp891ynmhws87c6z7yyv2li1v0ss9di40hxf57vh"))

(define rust-yoke-derive-0.8.2
  (crate-source "yoke-derive" "0.8.2"
                "13l5y5sz4lqm7rmyakjbh6vwgikxiql51xfff9hq2j485hk4r16y"))

(define rust-zerofrom-0.1.6
  (crate-source "zerofrom" "0.1.6"
                "19dyky67zkjichsb7ykhv0aqws3q0jfvzww76l66c19y6gh45k2h"))

(define rust-zerofrom-derive-0.1.6
  (crate-source "zerofrom-derive" "0.1.6"
                "00l5niw7c1b0lf1vhvajpjmcnbdp2vn96jg4nmkhq2db0rp5s7np"))

(define rust-zeroize-1.9.1
  (crate-source "zeroize" "1.9.1"
                "0yb8iykihpl3hfw5c4silw2lklpfxajkaa9yj1qw6jsy5hwq8c71"))

(define rust-zeroize-derive-1.5.0
  (crate-source "zeroize_derive" "1.5.0"
                "0a7kq8srk81pn23xqn7c9jw1jpnfy41ffn802x1zrqqgpdf6al1w"))

(define rust-zerotrie-0.2.5
  (crate-source "zerotrie" "0.2.5"
                "0gss16krjzk22m57dz5hkdjg99ibj6pa41qr68na7w1jpp1nk8jf"))

(define rust-zerovec-0.11.8
  (crate-source "zerovec" "0.11.8"
                "1n3xlvyba8riys9s8awy4xp533phqycr78nbsmvdkh86g3hn815v"))

(define rust-zerovec-derive-0.11.6
  (crate-source "zerovec-derive" "0.11.6"
                "1ni5j8v99x3fcf3l8kp64b7aq4vf8y22jshfq74xs9mxkp1nzprl"))

(define rust-zip-9.5.1
  (crate-source "zip" "9.5.1"
                "00m32ya87zdsjmsliq6qimz0yf7ggfmsqlbvxjqzb2mcjza6yiz3"))

(define rust-zlib-rs-0.5.5
  (crate-source "zlib-rs" "0.5.5"
                "1lxa1hf3bs8ip15jq8i8h9wdaaahcgxlzxvsj2vc5qmaa7fhx6a0"))

(define rust-zmij-1.0.10
  (crate-source "zmij" "1.0.10"
                "0s2q2k40d8gj81w039mmlxrzdbhz74l8x8xxch7q8gdfzggxiq1h"))

(define rust-zstd-0.14.3
  (crate-source "zstd" "0.14.3"
                "0w1wzfxmypnq1f1p7s0g8abn7ba6zkp7wxpggccl0vmgj2zyjfnv"))

(define rust-zstd-safe-8.2.0
  (crate-source "zstd-safe" "8.2.0"
                "0js3swa592j55g2b76vd6719h0qa5cdj48n7f6vskq99dgqj1mfy"))

(define rust-zstd-sys-2.1.2+zstd.1.5.7
  ;; TODO REVIEW: Check bundled sources.
  (crate-source "zstd-sys" "2.1.2+zstd.1.5.7"
                "1lsfr0hj9cp42lnfsnfriqcg1yvpj4jbbnxvnz5j68rvajfdl6j8"))

(define-cargo-inputs lookup-cargo-inputs
  (ty =>
      (list
       rust-adler2-2.0.1
       rust-aho-corasick-1.1.5
       rust-alloca-0.4.0
       rust-allocator-api2-0.2.21
       rust-anes-0.1.6
       rust-annotate-snippets-0.11.5
       rust-anstream-0.6.21
       rust-anstream-1.0.0
       rust-anstyle-1.0.14
       rust-anstyle-lossy-1.1.4
       rust-anstyle-parse-0.2.7
       rust-anstyle-parse-1.0.0
       rust-anstyle-query-1.1.4
       rust-anstyle-svg-0.1.11
       rust-anstyle-wincon-3.0.10
       rust-anyhow-1.0.104
       rust-approx-0.5.1
       rust-ar-archive-writer-0.5.3
       rust-arc-swap-1.9.3
       rust-argfile-1.0.0
       rust-arrayvec-0.7.8
       rust-assert-fs-1.1.4
       rust-async-trait-0.1.89
       rust-attribute-derive-0.11.0
       rust-attribute-derive-macro-0.11.0
       rust-autocfg-1.5.0
       rust-bit-set-0.8.0
       rust-bit-vec-0.8.0
       rust-bitflags-1.3.2
       rust-bitflags-2.13.2
       rust-bitvec-1.1.1
       rust-block-buffer-0.10.4
       rust-block2-0.6.2
       rust-boxcar-0.2.14
       rust-bstr-1.13.1
       rust-bumpalo-3.19.0
       rust-bytecheck-0.8.2
       rust-bytecheck-derive-0.8.2
       rust-bytes-1.12.0
       rust-cachedir-0.3.1
       rust-camino-1.2.6
       rust-cast-0.3.0
       rust-castaway-0.2.4
       rust-cc-1.2.65
       rust-cfg-if-1.0.4
       rust-cfg-aliases-0.2.1
       rust-chacha20-0.10.0
       rust-char-str-0.0.4
       rust-ciborium-0.2.2
       rust-ciborium-io-0.2.2
       rust-ciborium-ll-0.2.2
       rust-clap-4.6.7
       rust-clap-builder-4.6.7
       rust-clap-complete-4.5.58
       rust-clap-complete-command-0.6.1
       rust-clap-complete-nushell-4.5.8
       rust-clap-derive-4.6.7
       rust-clap-lex-1.0.0
       rust-clearscreen-5.0.0
       rust-codspeed-6.0.0
       rust-codspeed-criterion-compat-6.0.0
       rust-codspeed-criterion-compat-walltime-6.0.0
       rust-codspeed-divan-compat-6.0.0
       rust-codspeed-divan-compat-macros-6.0.0
       rust-codspeed-divan-compat-walltime-6.0.0
       rust-collection-literals-1.0.2
       rust-colorchoice-1.0.4
       rust-colored-3.1.1
       rust-compact-str-0.10.0
       rust-condtype-1.3.0
       rust-console-0.16.6
       rust-console-error-panic-hook-0.1.7
       rust-console-log-1.1.0
       rust-constant-time-eq-0.6.1
       rust-countme-3.0.1
       rust-cpufeatures-0.2.17
       rust-cpufeatures-0.3.0
       rust-crc32fast-1.5.0
       rust-criterion-0.8.2
       rust-criterion-plot-0.5.0
       rust-criterion-plot-0.8.2
       rust-crossbeam-0.8.5
       rust-crossbeam-channel-0.5.15
       rust-crossbeam-deque-0.8.6
       rust-crossbeam-epoch-0.9.18
       rust-crossbeam-queue-0.3.12
       rust-crossbeam-utils-0.8.21
       rust-crunchy-0.2.4
       rust-crypto-common-0.1.6
       rust-csv-1.4.0
       rust-csv-core-0.1.12
       rust-ctrlc-3.5.2
       rust-dashmap-6.2.1
       rust-datatest-stable-0.3.3
       rust-defmt-1.1.0
       rust-defmt-macros-1.1.0
       rust-defmt-parser-1.0.0
       rust-diff-0.1.13
       rust-difflib-0.4.0
       rust-digest-0.10.7
       rust-dirs-6.0.0
       rust-dirs-sys-0.5.0
       rust-dispatch2-0.3.0
       rust-displaydoc-0.2.5
       rust-divan-macros-0.1.17
       rust-drop-bomb-0.1.5
       rust-dunce-1.0.5
       rust-dyn-clone-1.0.20
       rust-either-1.15.0
       rust-encode-unicode-1.0.0
       rust-equivalent-1.0.2
       rust-errno-0.3.14
       rust-escape8259-0.5.3
       rust-escargot-0.5.15
       rust-etcetera-0.11.0
       rust-fancy-regex-0.14.0
       rust-fastrand-2.3.0
       rust-fern-0.7.1
       rust-filetime-0.2.29
       rust-find-msvc-tools-0.1.9
       rust-flate2-1.1.2
       rust-fnv-1.0.7
       rust-foldhash-0.1.5
       rust-foldhash-0.2.0
       rust-form-urlencoded-1.2.2
       rust-fs-err-3.3.0
       rust-fsevent-sys-4.1.0
       rust-funty-2.0.0
       rust-futures-core-0.3.32
       rust-futures-task-0.3.32
       rust-futures-util-0.3.32
       rust-gen-lsp-types-0.11.0
       rust-generic-array-0.14.7
       rust-get-size-derive2-0.11.1
       rust-get-size2-0.11.1
       rust-getrandom-0.2.16
       rust-getrandom-0.3.4
       rust-getrandom-0.4.2
       rust-glob-0.3.4
       rust-globset-0.4.20
       rust-globwalk-0.9.1
       rust-half-2.6.0
       rust-hashbrown-0.14.5
       rust-hashbrown-0.15.5
       rust-hashbrown-0.17.1
       rust-hashlink-0.12.0
       rust-heck-0.5.0
       rust-hermit-abi-0.5.2
       rust-html-escape-0.2.13
       rust-icu-collections-2.3.0
       rust-icu-locale-core-2.3.0
       rust-icu-normalizer-2.3.0
       rust-icu-normalizer-data-2.3.0
       rust-icu-properties-2.3.0
       rust-icu-properties-data-2.3.0
       rust-icu-provider-2.3.1
       rust-id-arena-2.3.0
       rust-idna-1.1.0
       rust-idna-adapter-1.2.1
       rust-ignore-0.4.33
       rust-imara-diff-0.2.0
       rust-imperative-1.0.7
       rust-indexmap-2.14.2
       rust-indicatif-0.18.6
       rust-indoc-2.0.8
       rust-inotify-0.11.0
       rust-inotify-sys-0.1.5
       rust-insta-1.49.0
       rust-insta-cmd-0.7.0
       rust-interpolator-0.5.0
       rust-intrusive-collections-0.10.1
       rust-inventory-0.3.24
       rust-is-macro-0.3.8
       rust-is-terminal-0.4.16
       rust-is-terminal-polyfill-1.70.1
       rust-itertools-0.10.5
       rust-itertools-0.13.0
       rust-itertools-0.14.0
       rust-itertools-0.15.0
       rust-itoa-1.0.15
       rust-jiff-0.2.38
       rust-jiff-core-0.1.0
       rust-jiff-static-0.2.38
       rust-jiff-tzdb-0.1.4
       rust-jiff-tzdb-platform-0.1.3
       rust-jobserver-0.1.34
       rust-jod-thread-1.0.0
       rust-js-sys-0.3.100
       rust-kqueue-1.1.1
       rust-kqueue-sys-1.0.4
       rust-lazy-static-1.5.0
       rust-leb128fmt-0.1.0
       rust-libc-0.2.191
       rust-libcst-1.9.0
       rust-libcst-derive-1.9.0
       rust-libm-0.2.16
       rust-libmimalloc-sys-0.1.49
       rust-libredox-0.1.10
       rust-libtest-mimic-0.8.1
       rust-libz-rs-sys-0.5.5
       rust-linux-raw-sys-0.12.1
       rust-litemap-0.8.0
       rust-lock-api-0.4.14
       rust-lock-api-0.5.0
       rust-log-0.4.34
       rust-lsp-server-0.10.0
       rust-manyhow-0.14.1
       rust-manyhow-macros-0.14.1
       rust-markdown-1.0.0
       rust-matchers-0.2.0
       rust-matchit-0.9.2
       rust-memchr-2.8.3
       rust-memoffset-0.9.1
       rust-mimalloc-0.1.52
       rust-minicov-0.3.8
       rust-minimal-lexical-0.2.1
       rust-miniz-oxide-0.8.9
       rust-mio-1.2.4
       rust-munge-0.4.7
       rust-munge-macro-0.4.7
       rust-natord-1.0.9
       rust-nix-0.31.3
       rust-nom-7.1.3
       rust-normalize-line-endings-0.3.0
       rust-notify-8.2.0
       rust-notify-types-2.0.0
       rust-nu-ansi-term-0.50.1
       rust-num-traits-0.2.19
       rust-objc2-0.6.3
       rust-objc2-encode-4.1.0
       rust-object-0.39.1
       rust-once-cell-1.21.4
       rust-once-cell-polyfill-1.70.1
       rust-oorandom-11.1.5
       rust-option-ext-0.2.0
       rust-ordermap-1.2.2
       rust-os-pipe-1.2.2
       rust-os-str-bytes-7.1.1
       rust-page-size-0.6.0
       rust-parking-lot-0.12.5
       rust-parking-lot-0.13.0
       rust-parking-lot-core-0.9.12
       rust-parking-lot-core-0.10.0
       rust-paste-1.0.15
       rust-path-absolutize-4.0.1
       rust-path-dedot-4.0.1
       rust-path-slash-0.2.1
       rust-pathdiff-0.2.3
       rust-peg-0.8.5
       rust-peg-macros-0.8.5
       rust-peg-runtime-0.8.5
       rust-pep440-rs-0.7.3
       rust-pep508-rs-0.9.2
       rust-percent-encoding-2.3.2
       rust-pest-2.8.2
       rust-pest-derive-2.8.2
       rust-pest-generator-2.8.2
       rust-pest-meta-2.8.2
       rust-phf-0.11.3
       rust-phf-0.13.1
       rust-phf-0.14.0
       rust-phf-codegen-0.11.3
       rust-phf-codegen-0.14.0
       rust-phf-generator-0.11.3
       rust-phf-generator-0.14.0
       rust-phf-shared-0.11.3
       rust-phf-shared-0.13.1
       rust-phf-shared-0.14.0
       rust-pin-project-lite-0.2.16
       rust-pkg-config-0.3.32
       rust-portable-atomic-1.13.1
       rust-portable-atomic-util-0.2.4
       rust-potential-utf-0.1.3
       rust-predicates-3.1.4
       rust-predicates-core-1.0.10
       rust-predicates-tree-1.0.13
       rust-pretty-assertions-1.4.1
       rust-prettyplease-0.2.37
       rust-proc-macro-crate-3.5.0
       rust-proc-macro-error-attr2-2.0.0
       rust-proc-macro-error2-2.0.1
       rust-proc-macro-utils-0.10.0
       rust-proc-macro2-1.0.107
       rust-psm-0.1.32
       rust-ptr-meta-0.3.1
       rust-ptr-meta-derive-0.3.1
       rust-pyproject-toml-0.13.7
       rust-quick-xml-0.41.0
       rust-quickcheck-1.1.0
       rust-quickcheck-macros-1.2.0
       rust-quote-1.0.47
       rust-quote-use-0.9.0
       rust-quote-use-macros-0.9.0
       rust-r-efi-5.3.0
       rust-r-efi-6.0.0
       rust-radium-0.7.0
       rust-rancor-0.1.1
       rust-rand-0.8.8
       rust-rand-0.10.3
       rust-rand-core-0.6.4
       rust-rand-core-0.10.0
       rust-rayon-1.12.0
       rust-rayon-core-1.13.0
       rust-redox-syscall-0.5.17
       rust-redox-syscall-0.9.5
       rust-redox-users-0.5.2
       rust-ref-cast-1.0.25
       rust-ref-cast-impl-1.0.25
       rust-regex-1.13.1
       rust-regex-automata-0.4.18
       rust-regex-lite-0.1.7
       rust-regex-syntax-0.8.11
       rust-rend-0.5.3
       rust-rkyv-0.8.18
       rust-rkyv-derive-0.8.18
       rust-ron-0.12.0
       rust-rust-stemmers-1.2.0
       rust-rustc-hash-2.1.3
       rust-rustc-stable-hash-0.1.2
       rust-rustix-1.1.5
       rust-rustversion-1.0.22
       rust-ryu-1.0.20
       rust-salsa-0.28.6
       rust-salsa-macro-rules-0.28.6
       rust-salsa-macros-0.28.6
       rust-same-file-1.0.6
       rust-schemars-1.2.3
       rust-schemars-derive-1.2.3
       rust-scopeguard-1.2.0
       rust-seahash-4.1.0
       rust-semver-1.0.27
       rust-serde-1.0.229
       rust-serde-wasm-bindgen-0.6.5
       rust-serde-core-1.0.229
       rust-serde-derive-1.0.229
       rust-serde-derive-internals-0.30.0
       rust-serde-json-1.0.154
       rust-serde-spanned-1.1.2
       rust-serde-test-1.0.177
       rust-sha2-0.10.9
       rust-sharded-slab-0.1.7
       rust-shellexpand-3.1.2
       rust-shlex-2.0.1
       rust-simdutf8-0.1.5
       rust-similar-2.7.0
       rust-similar-3.2.0
       rust-siphasher-1.0.1
       rust-slab-0.4.12
       rust-smallvec-1.16.3
       rust-snapbox-1.0.0
       rust-snapbox-macros-1.0.0
       rust-stable-deref-trait-1.2.0
       rust-stacker-0.1.25
       rust-static-assertions-1.1.0
       rust-statrs-0.18.0
       rust-strip-ansi-escapes-0.2.1
       rust-strsim-0.11.1
       rust-strum-0.28.0
       rust-strum-macros-0.28.0
       rust-supports-hyperlinks-3.2.0
       rust-syn-2.0.119
       rust-syn-3.0.9
       rust-synstructure-0.13.2
       rust-tap-1.0.1
       rust-tempfile-3.27.0
       rust-terminal-size-0.4.4
       rust-terminfo-0.9.0
       rust-termtree-0.5.1
       rust-test-case-3.4.0
       rust-test-case-core-3.4.0
       rust-test-case-macros-3.4.0
       rust-thin-vec-0.2.21
       rust-thiserror-1.0.69
       rust-thiserror-2.0.21
       rust-thiserror-impl-1.0.69
       rust-thiserror-impl-2.0.21
       rust-thread-local-1.1.9
       rust-tikv-jemalloc-sys-0.6.1+5.3.0-1-ge13ca993e8ccb9ba9847cc330696e02839f328f7
       rust-tikv-jemallocator-0.6.1
       rust-tinystr-0.8.4
       rust-tinytemplate-1.2.1
       rust-tinyvec-1.10.0
       rust-tinyvec-macros-0.1.1
       rust-toml-0.9.12+spec-1.1.0
       rust-toml-1.1.8+spec-1.1.0
       rust-toml-datetime-0.7.5+spec-1.1.0
       rust-toml-datetime-1.1.2+spec-1.1.0
       rust-toml-edit-0.25.12+spec-1.1.0
       rust-toml-parser-1.1.5+spec-1.1.0
       rust-toml-writer-1.1.3+spec-1.1.0
       rust-tracing-0.1.44
       rust-tracing-attributes-0.1.31
       rust-tracing-core-0.1.36
       rust-tracing-flame-0.2.0
       rust-tracing-indicatif-0.3.14
       rust-tracing-log-0.2.0
       rust-tracing-subscriber-0.3.23
       rust-typed-arena-2.0.2
       rust-typed-path-0.13.0
       rust-typeid-1.0.3
       rust-typenum-1.18.0
       rust-ucd-trie-0.1.7
       rust-unicode-id-0.3.6
       rust-unicode-ident-1.0.24
       rust-unicode-normalization-0.1.25
       rust-unicode-width-0.2.2
       rust-unicode-xid-0.2.6
       rust-unit-prefix-0.5.1
       rust-unscanny-0.1.0
       rust-url-2.5.8
       rust-urlencoding-2.1.3
       rust-utf8-width-0.1.7
       rust-utf8-iter-1.0.4
       rust-utf8parse-0.2.2
       rust-uuid-1.28.0
       rust-valuable-0.1.1
       rust-version-ranges-0.1.1
       rust-version-check-0.9.5
       rust-vt100-0.16.2
       rust-vte-0.14.1
       rust-vte-0.15.0
       rust-wait-timeout-0.2.1
       rust-walkdir-2.5.0
       rust-wasi-0.11.1+wasi-snapshot-preview1
       rust-wasip2-1.0.1+wasi-0.2.4
       rust-wasip3-0.4.0+wasi-0.3.0-rc-2026-01-06
       rust-wasm-bindgen-0.2.123
       rust-wasm-bindgen-futures-0.4.73
       rust-wasm-bindgen-macro-0.2.123
       rust-wasm-bindgen-macro-support-0.2.123
       rust-wasm-bindgen-shared-0.2.123
       rust-wasm-bindgen-test-0.3.73
       rust-wasm-bindgen-test-macro-0.3.73
       rust-wasm-bindgen-test-shared-0.2.123
       rust-wasm-encoder-0.244.0
       rust-wasm-metadata-0.244.0
       rust-wasmparser-0.244.0
       rust-web-sys-0.3.100
       rust-web-time-1.1.0
       rust-which-8.0.6
       rust-wild-2.2.1
       rust-winapi-0.3.9
       rust-winapi-i686-pc-windows-gnu-0.4.0
       rust-winapi-util-0.1.11
       rust-winapi-x86-64-pc-windows-gnu-0.4.0
       rust-windows-link-0.1.3
       rust-windows-link-0.2.1
       rust-windows-sys-0.52.0
       rust-windows-sys-0.59.0
       rust-windows-sys-0.60.2
       rust-windows-sys-0.61.2
       rust-windows-targets-0.52.6
       rust-windows-targets-0.53.3
       rust-windows-aarch64-gnullvm-0.52.6
       rust-windows-aarch64-gnullvm-0.53.0
       rust-windows-aarch64-msvc-0.52.6
       rust-windows-aarch64-msvc-0.53.0
       rust-windows-i686-gnu-0.52.6
       rust-windows-i686-gnu-0.53.0
       rust-windows-i686-gnullvm-0.52.6
       rust-windows-i686-gnullvm-0.53.0
       rust-windows-i686-msvc-0.52.6
       rust-windows-i686-msvc-0.53.0
       rust-windows-x86-64-gnu-0.52.6
       rust-windows-x86-64-gnu-0.53.0
       rust-windows-x86-64-gnullvm-0.52.6
       rust-windows-x86-64-gnullvm-0.53.0
       rust-windows-x86-64-msvc-0.52.6
       rust-windows-x86-64-msvc-0.53.0
       rust-winnow-0.7.13
       rust-winnow-1.0.0
       rust-wit-bindgen-0.46.0
       rust-wit-bindgen-0.51.0
       rust-wit-bindgen-core-0.51.0
       rust-wit-bindgen-rust-0.51.0
       rust-wit-bindgen-rust-macro-0.51.0
       rust-wit-component-0.244.0
       rust-wit-parser-0.244.0
       rust-writeable-0.6.4
       rust-wyz-0.5.1
       rust-yansi-1.0.1
       rust-yoke-0.8.3
       rust-yoke-derive-0.8.2
       rust-zerofrom-0.1.6
       rust-zerofrom-derive-0.1.6
       rust-zeroize-1.9.1
       rust-zeroize-derive-1.5.0
       rust-zerotrie-0.2.5
       rust-zerovec-0.11.8
       rust-zerovec-derive-0.11.6
       rust-zip-9.5.1
       rust-zlib-rs-0.5.5
       rust-zmij-1.0.10
       rust-zstd-0.14.3
       rust-zstd-safe-8.2.0
       rust-zstd-sys-2.1.2+zstd.1.5.7
       ) ; list
      ) ; ty
  ) ; define-cargo-inputs
