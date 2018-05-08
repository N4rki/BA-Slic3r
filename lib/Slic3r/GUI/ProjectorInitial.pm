# DLP Projector screen for the SLA (stereolitography) print process

package Slic3r::GUI::Projector;
use strict;
use warnings;
use File::Basename qw(basename dirname);
use Wx qw(:dialog :id :misc :sizer :systemsettings :bitmap :button :icon :filedialog wxTheApp);
use Wx::Event qw(EVT_BUTTON EVT_CLOSE EVT_TEXT_ENTER EVT_SPINCTRL EVT_SLIDER);
use base qw(Wx::Dialog Class::Accessor);
use utf8;

__PACKAGE__->mk_accessors(qw(config config2 manual_control_config screen controller _optgroups));

sub new {
    my ($class, $parent) = @_;
    my $self = $class->SUPER::new($parent, -1, "Projector for DLP", wxDefaultPosition, wxDefaultSize);
    $self->config2({
        display                 => 0,
        show_bed                => 1,
        invert_y                => 0,
        zoom                    => 100,
        exposure_time           => 2,
        bottom_exposure_time    => 7,
        settle_time             => 1.5,
        bottom_layers           => 3,
        z_lift                  => 5,
        z_lift_speed            => 8,
        offset                  => [0,0],
    });
    $self->manual_control_config({
        xy_travel_speed         => 130,
        z_travel_speed          => 10,
        temperature             => '',
        bed_temperature         => '',
    });
    
    my $ini = eval { Slic3r::Config->read_ini("$Slic3r::GUI::datadir/DLP.ini") };
    if ($ini) {
        foreach my $opt_id (keys %{$ini->{_}}) {
            my $value = $ini->{_}{$opt_id};
            if ($opt_id eq 'offset') {
                $value = [ split /,/, $value ];
            }
            $self->config2->{$opt_id} = $value;
        }
    }
    
    my $sizer = Wx::BoxSizer->new(wxVERTICAL);
    
    $self->config(Slic3r::Config->new_from_defaults(
        qw(serial_port serial_speed bed_shape start_gcode end_gcode z_offset)
    ));
    $self->config->apply(wxTheApp->{mainframe}->{slaconfig});
       
    
    my $on_change = sub {
        my ($opt_id, $value) = @_;
        
        $self->config2->{$opt_id} = $value;
        $self->screen->reposition;
        $self->show_print_time;
        
        my $serialized = {};
        foreach my $opt_id (keys %{$self->config2}) {
            my $value = $self->config2->{$opt_id};
            if (ref($value) eq 'ARRAY') {
                $value = join ',', @$value;
            }
            $serialized->{$opt_id} = $value;
        }
        Slic3r::Config->write_ini(
            "$Slic3r::GUI::datadir/DLP.ini",
            { _ => $serialized });
    };
    
    
    
    
    
   
    {
        # should be wxCLOSE but it crashes on Linux, maybe it's a Wx bug
        my $buttons = Wx::BoxSizer->new(wxHORIZONTAL);
        {
            my $btn = Wx::Button->new($self, -1, "Export SVG…");
            EVT_BUTTON($self, $btn, sub {
                $self->_export_svg;
            });
            $buttons->Add($btn, 0);
        }
        $buttons->AddStretchSpacer(1);
        {
            my $btn = Wx::Button->new($self, -1, "Close");
            $btn->SetDefault;
            EVT_BUTTON($self, $btn, sub {
                $self->_close;
            });
            $buttons->Add($btn, 0);
        }
        $sizer->Add($buttons, 0, wxEXPAND | wxBOTTOM | wxLEFT | wxRIGHT, 10);
    }
    EVT_CLOSE($self, sub {
        $self->_close;
    });
    
    $self->SetSizer($sizer);
    $sizer->SetSizeHints($self);
    
    # reuse existing screen if any
    if ($Slic3r::GUI::DLP_projection_screen) {
        $self->screen($Slic3r::GUI::DLP_projection_screen);
        $self->screen->config($self->config);
        $self->screen->config2($self->config2);
    } else {
        $self->screen(Slic3r::GUI::Projector::Screen->new($parent, $self->config, $self->config2));
        $Slic3r::GUI::DLP_projection_screen = $self->screen;
    }
    $self->screen->reposition;
    $self->screen->Show;
    wxTheApp->{mainframe}->Hide;
    
    # initialize controller
    $self->controller(Slic3r::GUI::Projector::Controller->new(
        config  => $self->config,
        config2 => $self->config2,
        screen  => $self->screen,
        on_project_layer => sub {
            my ($layer_num) = @_;
            
            $self->{layers_spinctrl}->SetValue($layer_num);
            $self->{layers_slider}->SetValue($layer_num);
            
            my $duration = $self->controller->remaining_print_time;
            $self->_set_status(sprintf "Printing layer %d/%d (z = %.2f); %d minutes and %d seconds left",
                $layer_num, $self->controller->_print->layer_count,
                $self->controller->current_layer_height,
                int($duration/60), ($duration - int($duration/60)*60));  # % truncates to integer
        },
        on_print_completed => sub {
            $self->_update_buttons;
            $self->_set_status('');
            Wx::Bell();
        },
    ));
    {
        my $max = $self->controller->_print->layer_count-1;
        $self->{layers_spinctrl}->SetRange(0, $max); 
        $self->{layers_slider}->SetRange(0, $max);
    }
    
    $self->_update_buttons;
    $self->show_print_time;
    
    return $self;
}

sub _update_buttons {
    my ($self) = @_;
    
    my $is_printing = $self->controller->is_printing;
    my $is_projecting = $self->controller->is_projecting;
    $self->{btn_manual_control}->Show(!$is_printing);
    $self->{btn_print}->Show(!$is_printing && !$is_projecting);
    $self->{btn_stop}->Show($is_printing || $is_projecting);
    $self->{layers_spinctrl}->Enable(!$is_printing);
    $self->{layers_slider}->Enable(!$is_printing);
    if ($is_printing) {
        $_->disable for @{$self->_optgroups};
    } else {
        $_->enable for @{$self->_optgroups};
    }
    $self->Layout;
}

sub _export_svg {
    my ($self) = @_;
    
    my $output_file = 'print.svg';
    my $dlg = Wx::FileDialog->new(
        $self,
        'Save SVG file as:',
        wxTheApp->output_path(dirname($output_file)),
        basename($output_file),
        &Slic3r::GUI::FILE_WILDCARDS->{svg},
        wxFD_SAVE | wxFD_OVERWRITE_PROMPT,
    );
    if ($dlg->ShowModal != wxID_OK) {
        $dlg->Destroy;
        return;
    }
    $output_file = Slic3r::decode_path($dlg->GetPath);
    
    $self->controller->_print->write_svg($output_file);
}

sub _set_status {
    my ($self, $status) = @_;
    $self->{status_text}->SetLabel($status // '');
    $self->{status_text}->Wrap($self->{status_text}->GetSize->GetWidth);
    $self->{status_text}->Refresh;
    $self->Layout;
}

sub show_print_time {
    my ($self) = @_;
    
    
    my $duration = $self->controller->print_time;
    $self->_set_status(sprintf "Estimated print time: %d minutes and %d seconds - %.2f liters",
        int($duration/60), ($duration - int($duration/60)*60),  # % truncates to integer
        $self->controller->total_resin);
}

sub _close {
    my $self = shift;
    
    # if projection screen is not on the same display as our dialog,
    # ask the user whether they want to keep it open
    my $keep_screen = 0;
    my $display_area = Wx::Display->new($self->config2->{display})->GetGeometry;
    if (!$display_area->Contains($self->GetScreenPosition)) {
        my $res = Wx::MessageDialog->new($self, "Do you want to keep the black screen open?", 'Black screen', wxYES_NO | wxYES_DEFAULT | wxICON_QUESTION)->ShowModal;
        $keep_screen = ($res == wxID_YES);
    }
    
    if ($keep_screen) {
        $self->screen->config(undef);
        $self->screen->config2(undef);
        $self->screen->Refresh;
    } else {
        $self->screen->Destroy;
        $self->screen(undef);
        $Slic3r::GUI::DLP_projection_screen = undef;
    }
    wxTheApp->{mainframe}->Show;
    
    $self->EndModal(wxID_OK);
}

package Slic3r::GUI::Projector::Controller;
use Moo;
use Wx qw(wxTheApp :id :timer);
use Wx::Event qw(EVT_TIMER);
use Slic3r::Geometry qw(unscale);
use Slic3r::Print::State ':steps';
use Time::HiRes qw(gettimeofday tv_interval);

has 'config'                => (is => 'ro', required => 1);
has 'config2'               => (is => 'ro', required => 1);
has 'screen'                => (is => 'ro', required => 1);
has 'on_project_layer'      => (is => 'rw');
has 'on_print_completed'    => (is => 'rw');
has 'sender'                => (is => 'rw');
has 'timer'                 => (is => 'rw');
has 'is_printing'           => (is => 'rw', default => sub { 0 });
has '_print'                => (is => 'rw');
has '_heights'              => (is => 'rw');
has '_layer_num'            => (is => 'rw');
has '_timer_cb'             => (is => 'rw');

sub BUILD {
    my ($self) = @_;
    
    Slic3r::GUI::disable_screensaver();
    
    # init print
    {
        my $print = Slic3r::SLAPrint->new(wxTheApp->{mainframe}->{plater}->{model});
        $print->apply_config(wxTheApp->{mainframe}->{plater}->config);
        $print->apply_config(wxTheApp->{mainframe}->{slaconfig});
        $self->_print($print);
        $self->screen->print($print);
    
        # make sure layers were sliced
        {
            my $progress_dialog = Wx::ProgressDialog->new('Slicing…', "Processing layers…", 100, undef, 0);
            $progress_dialog->Pulse;
            $print->slice;
            $progress_dialog->Destroy;
        }
        
        $self->_heights($print->heights);
    }
    
    # projection timer
    my $timer_id = &Wx::NewId();
    $self->timer(Wx::Timer->new($self->screen, $timer_id));
    EVT_TIMER($self->screen, $timer_id, sub {
        my $cb = $self->_timer_cb;
        $self->_timer_cb(undef);
        $cb->();
    });
}

sub delay {
    my ($self, $wait, $cb) = @_;
    
    $self->_timer_cb($cb);
    $self->timer->Start($wait * 1000, wxTIMER_ONE_SHOT);
}

sub current_layer_height {
    my ($self) = @_;
    
    return $self->_heights->[$self->_layer_num];
}

sub start_print {
    my ($self) = @_;
    
    {
        $self->sender(Slic3r::GCode::Sender->new);
        my $res = $self->sender->connect(
            $self->config->serial_port,
            $self->config->serial_speed,
        );
        if (!$res || !$self->sender->wait_connected) {
            Slic3r::GUI::show_error(undef, "Connection failed. Check serial port and speed.");
            return;
        }
        Slic3r::debugf "connected to " . $self->config->serial_port . "\n";
        
        # TODO: this wait should be handled by GCodeSender
        sleep 4;
        
        # send custom start G-code
        $self->sender->send($_, 1) for grep !/^;/, split /\n/, $self->config->start_gcode;
        $self->sender->("G90", 1); # set absolute positioning
    }
    
    $self->is_printing(1);
    
    # TODO: block until the G1 command has been performed
    # we could do this with M400 + M115 but maybe it's not portable
    $self->delay(5, sub {
        # start with black
        Slic3r::debugf "starting black projection\n";
        $self->_layer_num(-1);
        $self->screen->project_layer(undef);
        $self->delay($self->config2->{settle_time}, sub {
            $self->project_next_layer;
        });
    });
}

sub stop_print {
    my ($self) = @_;
    
    if ($self->sender) {
        $self->sender->disconnect;
    }
    
    $self->is_printing(0);
    $self->timer->Stop;
    $self->_timer_cb(undef);
    $self->screen->project_layer(undef);
}

sub print_completed {
    my ($self) = @_;
    
    # send custom end G-code
    if ($self->sender) {
        $self->sender->send($_, 1) for grep !/^;/, split /\n/, $self->config->end_gcode;
    }
    
    # call this before the on_print_completed callback otherwise buttons
    # won't be updated correctly
    $self->stop_print;
    
    $self->on_print_completed->()
        if $self->is_printing && $self->on_print_completed;
}

sub is_projecting {
    my ($self) = @_;
    
    return defined $self->screen->layer_num;
}

sub project_layer {
    my ($self, $layer_num) = @_;
    
    if (!defined $layer_num || $layer_num >= $self->_print->layer_count) {
        $self->screen->project_layer(undef);
        return;
    }
    
    $self->screen->project_layer($layer_num);
}

sub project_next_layer {
    my ($self) = @_;
    
    $self->_layer_num($self->_layer_num + 1);
    Slic3r::debugf "projecting layer %d\n", $self->_layer_num;
    if ($self->_layer_num >= $self->_print->layer_count) {
        $self->print_completed;
        return;
    }
    
    $self->on_project_layer->($self->_layer_num) if $self->on_project_layer;
    
    if ($self->sender) {
        my $z = $self->current_layer_height + $self->config->z_offset;
        my $F = $self->config2->{z_lift_speed} * 60;
        if ($self->config2->{z_lift} != 0) {
            $self->sender->send(sprintf("G1 Z%.5f F%d", $z + $self->config2->{z_lift}, $F), 1);
        }
        $self->sender->send(sprintf("G1 Z%.5f F%d", $z, $F), 1);
    }
    
    # TODO: we should block until G1 commands have been performed, see note below
    $self->delay($self->config2->{settle_time}, sub {
        $self->project_layer($self->_layer_num);
        
        # get exposure time
        my $time = $self->config2->{exposure_time};
        if ($self->_layer_num < $self->config2->{bottom_layers}) {
            $time = $self->config2->{bottom_exposure_time};
        }
        
        $self->delay($time, sub {
            $self->screen->project_layer(undef);
            $self->project_next_layer;
        });
    });
}

sub remaining_print_time {
    my ($self) = @_;
    
    my $remaining_layers = @{$self->_heights} - $self->_layer_num;
    my $remaining_bottom_layers = $self->_layer_num >= $self->config2->{bottom_layers}
        ? 0
        : $self->config2->{bottom_layers} - $self->_layer_num;
    
    return $remaining_bottom_layers * $self->config2->{bottom_exposure_time}
        + ($remaining_layers - $remaining_bottom_layers) * $self->config2->{exposure_time}
        + $remaining_layers * $self->config2->{settle_time};
}

sub print_time {
    my ($self) = @_;
    
    return $self->config2->{bottom_layers} * $self->config2->{bottom_exposure_time}
        + ($self->_print->layer_count - $self->config2->{bottom_layers}) * $self->config2->{exposure_time}
        + $self->_print->layer_count * $self->config2->{settle_time};
}

sub total_resin {
    my ($self) = @_;
    
    my $vol = 0;  # mm^3
    
    for my $i (0..($self->_print->layer_count-1)) {
        my $lh = $self->_heights->[$i] - ($i == 0 ? 0 : $self->_heights->[$i-1]);
        $vol += unscale(unscale($_->area)) * $lh for @{ $self->_print->layer_slices($i) };
    }
    
    return $vol/1000/1000;  # liters
}

sub DESTROY {
    my ($self) = @_;
    
    $self->timer->Stop if $self->timer;
    $self->sender->disconnect if $self->sender;
    Slic3r::GUI::enable_screensaver();
}

package Slic3r::GUI::Projector::Screen;
use Wx qw(:dialog :id :misc :sizer :colour :pen :brush :font wxBG_STYLE_CUSTOM);
use Wx::Event qw(EVT_PAINT EVT_SIZE);
use base qw(Wx::Dialog Class::Accessor);

use List::Util qw(min);
use Slic3r::Geometry qw(X Y unscale scale);
use Slic3r::Geometry::Clipper qw(intersection_pl union_ex);

__PACKAGE__->mk_accessors(qw(config config2 scaling_factor bed_origin print layer_num));

sub new {
    my ($class, $parent, $config, $config2) = @_;
    my $self = $class->SUPER::new($parent, -1, "Projector", wxDefaultPosition, wxDefaultSize, 0);
    
    $self->config($config);
    $self->config2($config2);
    $self->SetBackgroundStyle(wxBG_STYLE_CUSTOM);
    EVT_SIZE($self, \&_resize);
    EVT_PAINT($self, \&_repaint);
    $self->_resize;
    
    return $self;
}

sub reposition {
    my ($self) = @_;
    
    my $display = Wx::Display->new($self->config2->{display});
    my $area = $display->GetGeometry;
    $self->Move($area->GetPosition);
    # ShowFullScreen doesn't use the right screen
    #$self->ShowFullScreen($self->config2->{fullscreen});
    $self->SetSize($area->GetSize);
    $self->_resize;
    $self->Refresh;
}

sub _resize {
    my ($self) = @_;
    
    return if !$self->config;
    my ($cw, $ch) = $self->GetSizeWH;
    
    # get bed shape polygon
    my $bed_polygon = Slic3r::Polygon->new_scale(@{$self->config->bed_shape});
    my $bb = $bed_polygon->bounding_box;
    my $size = $bb->size;
    my $center = $bb->center;

    # calculate the scaling factor needed for constraining print bed area inside preview
    # scaling_factor is expressed in pixel / mm
    $self->scaling_factor(min($cw / unscale($size->x), $ch / unscale($size->y))); #)
    
    # apply zoom to scaling factor
    if ($self->config2->{zoom} != 0) {
        # TODO: make sure min and max in the option config are enforced
        $self->scaling_factor($self->scaling_factor * ($self->config2->{zoom}/100));
    }
    
    # calculate the displacement needed for centering bed on screen
    $self->bed_origin([
        $cw/2 - (unscale($center->x) - $self->config2->{offset}->[X]) * $self->scaling_factor,
        $ch/2 - (unscale($center->y) - $self->config2->{offset}->[Y]) * $self->scaling_factor,  #))
    ]);
    
    $self->Refresh;
}

sub project_layer {
    my ($self, $layer_num) = @_;
    
    $self->layer_num($layer_num);
    $self->Refresh;
}

sub _repaint {
    my ($self) = @_;
    
    my $dc = Wx::AutoBufferedPaintDC->new($self);
    my ($cw, $ch) = $self->GetSizeWH;
    return if $cw == 0;  # when canvas is not rendered yet, size is 0,0
    
    $dc->SetPen(Wx::Pen->new(wxBLACK, 1, wxSOLID));
    $dc->SetBrush(Wx::Brush->new(wxBLACK, wxSOLID));
    $dc->DrawRectangle(0, 0, $cw, $ch);
    
    return if !$self->config;
    
    # turn size into max visible coordinates
    # TODO: or should we use ClientArea?
    $cw--;
    $ch--;
    
    # draw bed
    if ($self->config2->{show_bed}) {
        $dc->SetPen(Wx::Pen->new(wxRED, 2, wxSOLID));
        $dc->SetBrush(Wx::Brush->new(wxGREEN, wxTRANSPARENT));
        
        # draw contour
        my $bed_polygon = Slic3r::Polygon->new_scale(@{$self->config->bed_shape});
        $dc->DrawPolygon($self->scaled_points_to_pixel($bed_polygon), 0, 0);
        
        # draw grid
        $dc->SetPen(Wx::Pen->new(wxRED, 1, wxSOLID));
        {
            my $bb = $bed_polygon->bounding_box;
            my $step = scale 10;  # 1cm grid
            my @polylines = ();
            for (my $x = $bb->x_min - ($bb->x_min % $step) + $step; $x < $bb->x_max; $x += $step) {
                push @polylines, Slic3r::Polyline->new([$x, $bb->y_min], [$x, $bb->y_max]);
            }
            for (my $y = $bb->y_min - ($bb->y_min % $step) + $step; $y < $bb->y_max; $y += $step) {
                push @polylines, Slic3r::Polyline->new([$bb->x_min, $y], [$bb->x_max, $y]);
            }
            $dc->DrawLine(map @$_, @$_)
                for map $self->scaled_points_to_pixel([ @$_[0,-1] ]),
                    @{intersection_pl(\@polylines, [$bed_polygon])};
        }
        
        # draw axes orientation
        $dc->SetPen(Wx::Pen->new(wxWHITE, 4, wxSOLID));
        {
            foreach my $endpoint ([10, 0], [0, 10]) {
                $dc->DrawLine(
                    map @{$self->unscaled_point_to_pixel($_)}, [0,0], $endpoint
                );
            }
            
            $dc->SetTextForeground(wxWHITE);
            $dc->SetFont(Wx::Font->new(20, wxDEFAULT, wxNORMAL, wxNORMAL));
            
            my $p = $self->unscaled_point_to_pixel([10, 0]);
            $dc->DrawText("X", $p->[X], $p->[Y]);
            $p = $self->unscaled_point_to_pixel([0, 10]);
            $dc->DrawText("Y", $p->[X]-20, $p->[Y]-10);
        }
    }
    
    # get layers at this height
    # draw layers
    $dc->SetPen(Wx::Pen->new(wxWHITE, 1, wxSOLID));
    
    return if !$self->print || !defined $self->layer_num;
    
    if ($self->print->layer_solid($self->layer_num)) {
        $self->_paint_expolygon($_, $dc) for @{$self->print->layer_slices($self->layer_num)};
    } else {
        # perimeters first, because their "hole" is painted black
        $self->_paint_expolygon($_, $dc) for
            @{$self->print->layer_perimeters($self->layer_num)},
            @{$self->print->layer_solid_infill($self->layer_num)};
        
        $self->_paint_expolygon($_, $dc)
            for @{union_ex($self->print->layer_infill($self->layer_num)->grow)};
    }
    
    # draw support material
    my $sm_radius = $self->print->config->get_abs_value_over('support_material_extrusion_width', $self->print->config->layer_height)/2;
    $dc->SetBrush(Wx::Brush->new(wxWHITE, wxSOLID));
    foreach my $pillar (@{$self->print->sm_pillars}) {
        next unless $pillar->{top_layer}    >= $self->layer_num
                 && $pillar->{bottom_layer} <= $self->layer_num;
        
        my $radius = min(
            $sm_radius,
            ($pillar->{top_layer} - $self->layer_num + 1) * $self->print->config->layer_height,
        );
        
        $dc->DrawCircle(
            @{$self->scaled_points_to_pixel([$pillar->{point}])->[0]},
            $radius * $self->scaling_factor,
        );
    }
}

sub _paint_expolygon {
    my ($self, $expolygon, $dc) = @_;
    
    my @polygons = sort { $a->contains_point($b->first_point) ? -1 : 1 } @$expolygon;
    $self->_paint_polygon($_, $dc) for @polygons;
}

sub _paint_polygon {
    my ($self, $polygon, $dc) = @_;
    
    if ($polygon->is_counter_clockwise) {
        $dc->SetBrush(Wx::Brush->new(wxBLUE, wxSOLID));
    } else {
        $dc->SetBrush(Wx::Brush->new(wxBLACK, wxSOLID));
    }
    $dc->DrawPolygon($self->scaled_points_to_pixel($polygon->pp), 0, 0);
}

# convert a model coordinate into a pixel coordinate
sub unscaled_point_to_pixel {
    my ($self, $point) = @_;
    
    my $zero = $self->bed_origin;
    my $p = [
        $point->[X] * $self->scaling_factor + $zero->[X],
        $point->[Y] * $self->scaling_factor + $zero->[Y],
    ];
    
    if (!$self->config2->{invert_y}) {
        my $ch = $self->GetSize->GetHeight;
        $p->[Y] = $ch - $p->[Y];
    }
    
    return $p;
}

sub scaled_points_to_pixel {
    my ($self, $points) = @_;
    
    return [
        map $self->unscaled_point_to_pixel($_),
            map Slic3r::Pointf->new_unscale(@$_),
            @$points
    ];
}

1;
