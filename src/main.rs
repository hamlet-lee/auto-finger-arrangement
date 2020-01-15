use crate::Finger::First;

#[derive(Debug, Clone)]
struct FingerStatus {
    finger_status_list: Vec<FingerStringAndPose>
}
#[derive(Debug, Clone)]
struct FingerStringAndPose {
    finger: Finger,
    violin_string: ViolinString,
    action: Pose
}

#[derive(Debug, Clone, PartialOrd, PartialEq)]
enum Pose {
    LIFT,
    DOWN,
    APPROACHING
}

#[derive(Debug, Clone)]
struct StageNodes {
    nodes: Vec<FingerStatusAndChoice>,
}

#[derive(Debug, Clone)]
struct FingerStatusAndChoice {
    finger_status: FingerStatus,
    choice: Choice
}

#[derive(Debug, Clone)]
struct Choice {
    prev_pos: i32,
    accumulate_cost: f32
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
enum Finger {
    First,
    Second,
    Third,
    Fourth
}

#[derive(Debug, Clone, PartialEq)]
enum ViolinString {
    G, D, A, E
}

#[derive(Debug, Clone)]
struct Note {
    finger: Option<Finger>,
    violin_string: ViolinString
}

fn enumerate_violin_string_and_action(allow_approaching: bool) -> Vec<(ViolinString, Pose)> {
    let mut ret_vec = Vec::new();
    for violin_string in &[ViolinString::G, ViolinString::D, ViolinString::A, ViolinString::E] {
        let pose_list = if allow_approaching {
            vec!(Pose::DOWN, Pose::LIFT, Pose::APPROACHING)
        } else {
            vec!(Pose::DOWN, Pose::LIFT)
        };
        for action in pose_list {
            ret_vec.push( (violin_string.clone(), action.clone()) );
        }
    }
    ret_vec
}


fn enumerate_string_action_for_four(allow_approach: bool) -> Vec<Vec<(ViolinString,Pose)>> {
    let mut v : Vec<(ViolinString, Pose)> = Vec::new();
    for i in 0..4 {
        v.push((ViolinString::G, Pose::LIFT));
    }

    let mut ret : Vec<Vec<(ViolinString,Pose)>> = Vec::new();
    fill(&mut v, 0, & mut ret, allow_approach);
    ret
}

fn fill(mut v: & mut Vec<(ViolinString, Pose)>,
        idx: i32,
        out: & mut Vec<Vec<(ViolinString, Pose)>>,
        allow_approach: bool) {

    if idx >= 4 {
        out.push(v.clone());
        return;
    } else {
        let enum_of_string_action : Vec<(ViolinString, Pose)> = enumerate_violin_string_and_action(allow_approach);
        for string_action in enum_of_string_action {
            v[idx as usize] = (string_action.0, string_action.1);
            fill(&mut v, idx + 1, out, allow_approach);
        }
    }
}

fn down_finger_no_disturb(finger: Finger, playing: Option<Finger>) -> bool {
    match playing {
        None => false,
        Some(pf) => finger <= pf
    }
}

fn create_finger_status_list_playing_note(
    playing_note: &Note,
    allow_approaching: bool) -> Vec<FingerStatus> {

    let possibles = enumerate_string_action_for_four(allow_approaching);
    let vfap : Vec<FingerStatus> = possibles.into_iter().map(|x| {
        let mut v : Vec<FingerStringAndPose> = Vec::new();
        let flist = &[Finger::First, Finger::Second, Finger::Third, Finger::Fourth];
        for i in (0..4) {
            let fap = FingerStringAndPose{
                finger: flist[i].clone(),
                violin_string: x[0].0.clone(),
                action: x[0].1.clone()
            };
            v.push(fap);
        }
        FingerStatus {
            finger_status_list: v
        }
    }).collect();

    // deal with playing note
    let r = vfap.into_iter().filter( |fs| {
        fs.finger_status_list.iter().any(|fsp| {
            match &playing_note.finger {
                None => true,
                Some(pf) => *pf == fsp.finger && fsp.action == Pose::DOWN
                    && fsp.violin_string == playing_note.violin_string
            }
        })
        && fs.finger_status_list.iter().all(|fsp| {
            match fsp.action {
                Pose::LIFT | Pose::APPROACHING => true,
                Pose::DOWN => {
                    if fsp.violin_string != playing_note.violin_string {
                        true
                    } else {
                        down_finger_no_disturb(fsp.finger.clone(), playing_note.finger.clone())
                    }
                }
            }
        })
    }).collect();
    return r;
//    vec!(FingerStatus {finger_status_list: vec!(FingerStringAndPose{
//        finger: Finger::First,
//        violin_string: ViolinString::G,
//        action: Pose::LIFT
//    })})
}
fn find_best_way (from:&StageNodes, to: & mut StageNodes) -> () {
    for i in 0..to.nodes.len() {
        to.nodes[i].choice.prev_pos = 0;
        to.nodes[i].choice.accumulate_cost = from.nodes[0].choice.accumulate_cost + 0.1;
    }
}

fn compute_transition(from: &FingerStatus, to: &FingerStatus) -> String {
    "xxx".to_string()
}
fn main() {
    let score = vec!(
        Note{ finger:None, violin_string: ViolinString::G},
        Note{ finger:Some(Finger::First), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Third), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Fourth), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Third), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::Second), violin_string: ViolinString::G},
        Note{ finger:Some(Finger::First), violin_string: ViolinString::G},
        Note{ finger:None, violin_string: ViolinString::G},
    );

    let mut stages:Vec<StageNodes> = Vec::new();
    let mut pos = 0;
    println!("Planning for score: {:#?}", score);
    for note in score.clone() {
        let vec_finger_status: Vec<FingerStatus>
            = create_finger_status_list_playing_note(
            &note, false);
        let nodes: Vec<_> = vec_finger_status.iter().map( | f |
            FingerStatusAndChoice {
                finger_status: f.clone(),
                choice: Choice {prev_pos: -1, accumulate_cost: 0.0 }
        }).collect();
        stages.push(StageNodes{nodes});
        if pos > 0 {
            // https://stackoverflow.com/questions/26409316/how-do-i-extract-two-mutable-elements-from-a-vec-in-rust
            let copy_from;
            {
                let from = stages[pos - 1].clone() ;
                copy_from = from;
            }
            let to = &mut stages[pos];
            find_best_way(&copy_from,
                          to);
        }
        pos = pos + 1;

        let vec_finger_status: Vec<FingerStatus>
            = create_finger_status_list_playing_note(
            &note, true);
        let nodes: Vec<FingerStatusAndChoice> = vec_finger_status.iter().map( | f | FingerStatusAndChoice {
            finger_status: f.clone(),
            choice: Choice {prev_pos: -1, accumulate_cost: 0.0 }
        }).collect();
        stages.push(StageNodes{nodes});

        let copy_from;
        {
            let from = stages.get(pos - 1).unwrap().clone();
            copy_from = from;
        }
        let to = stages.get_mut(pos).unwrap();

        find_best_way(&copy_from,
                          to);
        pos = pos + 1;
    }
    println!("Computing done!");

    // find best solution backward
    let mut choice_pos_array = vec!(0; pos);
    for cur in (0..pos).rev() {
        let mut min_cost = 10000.0;
        let mut min_cost_pos: i32= -1;
        let stage = stages.get(cur).unwrap();
        if cur == pos - 1 {
            // look for final choice
            for i in 0..stage.nodes.len() {
                let cost = stage.nodes.get(i).unwrap().choice.accumulate_cost;
                if cost < min_cost {
                    min_cost_pos = i as i32;
                    min_cost = cost;
                }
            }
            choice_pos_array[cur] = min_cost_pos;
        } else {
            // look for other choice
            let stage_nodes: &StageNodes = stages.get(cur+1).unwrap();
            let back_pos = choice_pos_array[cur + 1];
            let node : &FingerStatusAndChoice = stage_nodes.nodes.get(back_pos as usize).unwrap();
            choice_pos_array[cur] = node.choice.prev_pos;
        }
    }

    for cur in 0..pos {
        let node : &FingerStatusAndChoice = stages.get(cur).unwrap().nodes.get(choice_pos_array[cur] as usize).unwrap();
        if cur == 0 {
            // starting pose
            println!("starting with status {:#?}\n play note {} = {:?}",
                     node.finger_status, cur / 2 + 1, score.get( cur / 2));
        } else {
            // transition
            let node_from: &FingerStatusAndChoice = stages.get(cur as usize).unwrap().nodes.get(choice_pos_array[cur-1] as usize).unwrap();
            let transition = compute_transition(&node_from.finger_status, &node.finger_status);
            println!(" with transition: {:#?} to status {:#?}, and then ", transition, node.finger_status);
        }

        if cur % 2 == 1 {
            println!("play note {} = {:?} ", cur / 2 + 1, score.get(cur / 2))
        } else {
            println!("as middle action")
        }
//        let transition = analyze_transition()
    }
}
