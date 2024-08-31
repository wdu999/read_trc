# to decode Lecroy binary wfm (.trc)
#
# may not work well if COMM_ORDER is Hi First
# however, it looks like scope always save wfm with Lo First even set it to Hi First
# so probably not a concern
#
# reference:
# http://teledynelecroy.com/doc/docview.aspx?id=5891
# http://forums.ni.com/attachments/ni/60/4652/2/LeCroyWaveformTemplate_2_3.pdf)
# https://github.com/michael-betz/readTrc

library(tidyverse)
library(plotly)
library(R6)

Trc <- R6Class(
  "Trc",
  private = list(),
  public = list(
    record_type = c(
      "single_sweep", "interleaved", "histogram", "graph",
      "filter_coefficient", "complex", "extrema",
      "sequence_obsolete", "centered_RIS", "peak_detect"
    ),
    processing_done = c(
      "no_processing", "fir_filter", "interpolated", "sparsed",
      "autoscaled", "no_result", "rolling", "cumulative"
    ),
    timebase = c(
      "1_ps/div", "2_ps/div", "5_ps/div", "10_ps/div", "20_ps/div",
      "50_ps/div", "100_ps/div", "200_ps/div", "500_ps/div", "1_ns/div",
      "2_ns/div", "5_ns/div", "10_ns/div", "20_ns/div", "50_ns/div",
      "100_ns/div", "200_ns/div", "500_ns/div", "1_us/div", "2_us/div",
      "5_us/div", "10_us/div", "20_us/div", "50_us/div", "100_us/div",
      "200_us/div", "500_us/div", "1_ms/div", "2_ms/div", "5_ms/div",
      "10_ms/div", "20_ms/div", "50_ms/div", "100_ms/div", "200_ms/div",
      "500_ms/div", "1_s/div", "2_s/div", "5_s/div", "10_s/div",
      "20_s/div", "50_s/div", "100_s/div", "200_s/div", "500_s/div",
      "1_ks/div", "2_ks/div", "5_ks/div", "EXTERNAL"
    ),
    vert_coupling = c("DC_50_Ohms", "ground", "DC_1MOhm", "ground", "AC,_1MOhm"),
    fixed_vert_gain = c(
      "1_uV/div", "2_uV/div", "5_uV/div", "10_uV/div", "20_uV/div",
      "50_uV/div", "100_uV/div", "200_uV/div", "500_uV/div", "1_mV/div",
      "2_mV/div", "5_mV/div", "10_mV/div", "20_mV/div", "50_mV/div",
      "100_mV/div", "200_mV/div", "500_mV/div", "1_V/div", "2_V/div",
      "5_V/div", "10_V/div", "20_V/div", "50_V/div", "100_V/div",
      "200_V/div", "500_V/div", "1_kV/div"
    ),
    bandwidth_limit = c("off", "on"),
    wave_source = c("CHANNEL_1", "CHANNEL_2", "CHANNEL_3", "CHANNEL_4", "UNKNOWN"),
    meta = list(
      TEMPLATE_NAME = NULL,
      COMM_TYPE = NULL,
      COMM_ORDER = NULL,
      WAVE_DESC_LENGTH = NULL,
      USER_TEXT_LENGTH = NULL,
      TRIG_TIME_ARRAY = NULL,
      RIS_TIME_ARRAY = NULL,
      WAVE_ARRAY_1 = NULL,
      WAVE_ARRAY_2 = NULL,
      INSTRUMENT_NAME = NULL,
      INSTRUMENT_NUMBER = NULL,
      TRACE_LABEL = NULL,
      WAVE_ARRAY_COUNT = NULL,
      POINTS_PER_SCREEN = NULL,
      FIRST_VALID = NULL,
      LAST_VALID = NULL,
      FIRST_POINT = NULL,
      SPARSING_FACTOR = NULL,
      SEGMENT_NO = NULL,
      SUBARRAY_COUNT = NULL,
      SWEEPS_PER_ACQ = NULL,
      POINTS_PER_PAIR = NULL,
      PAIR_OFFSET = NULL,
      VERTICAL_GAIN = NULL,
      VERTICAL_OFFSET = NULL,
      MAX_VALUE = NULL,
      MIN_VALUE = NULL,
      NOMINAL_BITS = NULL,
      NOM_SUBARRAY_COUNT = NULL,
      HORIZONTAL_INTERVAL = NULL,
      HORIZONTAL_OFFSET = NULL,
      PIXEL_OFFSET = NULL,
      VERTUNIT = NULL,
      HORUNIT = NULL,
      HORIZ_UNCERTAINTY = NULL,
      TRIGGER_TIME = NULL,
      ACQ_DURATION = NULL,
      CA_RECORD_TYPE = NULL,
      PROCESSING_DONE = NULL,
      RIS_SWEEPS = NULL,
      TIME_BASE = NULL,
      VERTICAL_COUPLING = NULL,
      PROBE_ATTENUATION = NULL,
      FIXED_VERTICAL_GAIN = NULL,
      BAND_WIDTH_LIMIT = NULL,
      VERTICAL_VERNIER = NULL,
      ACQ_VERTICAL_OFFSET = NULL,
      WAVE_SOURCE = NULL
    ),
    trc_file = NULL,
    offset = NULL,
    data_format_size = 2, # 8 or 16 bit sample format
    endian = "little",
    raw = NULL,
    wfm = NULL,
    initialize = function() {},
    print = function(...) {
      message()

      n <- max(nchar(names(self$meta)))

      s <- str_c(
        str_pad("trc_file", n, "left"),
        " : ",
        self$trc_file
      )
      message(s)

      for (i in 1:length(self$meta)) {
        s <- str_c(
          str_pad(names(self$meta)[i], n, "left"),
          " : ",
          self$meta[[i]]
        )
        message(s)
      }
      invisible(self)
    },
    rd = function(t, i, l) {
      raw_slice <- self$raw[seq(self$offset + i, self$offset + i + l - 1)]

      if (t == "string") {
        return(readBin(raw_slice, character(), endian = self$endian))
      } else if (t %in% c("short", "long")) {
        return(readBin(raw_slice, integer(), size = l, endian = self$endian))
      } else if (t %in% c("float", "double")) {
        return(readBin(raw_slice, double(), size = l, endian = self$endian))
      }
    },
    get_timestamp = function(i, l) {
      raw_slice <- self$raw[seq(self$offset + i, self$offset + i + l - 1)]

      sec <- readBin(raw_slice[1:8], double(), size = 8, endian = self$endian)
      min <- readBin(raw_slice[9], integer(), size = 1)
      hour <- readBin(raw_slice[10], integer(), size = 1)
      day <- readBin(raw_slice[11], integer(), size = 1)
      month <- readBin(raw_slice[12], integer(), size = 1)
      year <- readBin(raw_slice[13:14], integer(), size = 2, endian = self$endian)

      return(make_datetime(year, month, day, hour, min, as.integer(sec)))
    },
    get_y = function() {
      i <- self$offset + self$meta$WAVE_DESC_LENGTH + self$meta$USER_TEXT_LENGTH + self$meta$TRIG_TIME_ARRAY + self$meta$RIS_TIME_ARRAY
      raw_slice <- self$raw[seq(i, i + self$meta$WAVE_ARRAY_1 - 1)]
      y <- readBin(raw_slice, integer(), n = self$meta$WAVE_ARRAY_COUNT, size = self$data_format_size, endian = self$endian)
      return(y)
    },
    decode = function(trc_file) {
      self$trc_file <- trc_file

      fp <- file(self$trc_file, "rb")
      self$raw <- readBin(fp, raw(), n = file.size(self$trc_file), size = 1)
      close(fp)

      # offset to start of WAVEDESC block
      self$offset <- str_locate(readBin(self$raw[1:64], character()), "WAVEDESC")[[1]]

      self$meta$TEMPLATE_NAME <- self$rd("string", 16, 16)
      self$meta$COMM_TYPE <- ifelse(self$rd("short", 32, 2) == 0, "byte", "word")
      self$data_format_size <- 1 + self$rd("short", 32, 2)

      # -------------------------------------------------------------------------
      # Data Type     | COMMM ORDER HI              | COMM ORDER LO
      # Word          | <MSB><LSB>                  | <LSB><MSB>
      # Long or Float | <MSB><byte2><byte3><LSB>    | <LSB><byte3><byte2><MSB>
      # Double        | <MSB><byte2>...<byte7><LSB> | <LSB><byte7>...<byte2><MSB>
      # -------------------------------------------------------------------------
      self$meta$COMM_ORDER <- ifelse(self$rd("short", 34, 2) == 0, "Hi First", "Lo First")
      self$endian <- ifelse(self$rd("short", 34, 2) == 0, "big", "little")

      self$meta$WAVE_DESC_LENGTH <- self$rd("long", 36, 4)
      self$meta$USER_TEXT_LENGTH <- self$rd("long", 40, 4)
      self$meta$TRIG_TIME_ARRAY <- self$rd("long", 48, 4)
      self$meta$RIS_TIME_ARRAY <- self$rd("long", 52, 4)
      self$meta$WAVE_ARRAY_1 <- self$rd("long", 60, 4)
      self$meta$WAVE_ARRAY_2 <- self$rd("long", 64, 4)

      self$meta$INSTRUMENT_NAME <- self$rd("string", 76, 16)
      self$meta$INSTRUMENT_NUMBER <- self$rd("long", 92, 4)
      self$meta$TRACE_LABEL <- self$rd("string", 96, 16)
      self$meta$WAVE_ARRAY_COUNT <- self$rd("long", 116, 4)
      self$meta$POINTS_PER_SCREEN <- self$rd("long", 120, 4)
      self$meta$FIRST_VALID <- self$rd("long", 124, 4)
      self$meta$LAST_VALID <- self$rd("long", 128, 4)
      self$meta$FIRST_POINT <- self$rd("long", 132, 4)
      self$meta$SPARSING_FACTOR <- self$rd("long", 136, 4)
      self$meta$SEGMENT_NO <- self$rd("long", 140, 4)
      self$meta$SUBARRAY_COUNT <- self$rd("long", 144, 4)
      self$meta$SWEEPS_PER_ACQ <- self$rd("long", 148, 4)
      self$meta$POINTS_PER_PAIR <- self$rd("short", 152, 2)
      self$meta$PAIR_OFFSET <- self$rd("short", 154, 2)
      self$meta$VERTICAL_GAIN <- self$rd("float", 156, 4)
      self$meta$VERTICAL_OFFSET <- self$rd("float", 160, 4)
      self$meta$MAX_VALUE <- self$rd("float", 164, 4)
      self$meta$MIN_VALUE <- self$rd("float", 168, 4)
      self$meta$NOMINAL_BITS <- self$rd("short", 172, 2)
      self$meta$NOM_SUBARRAY_COUNT <- self$rd("short", 174, 2)
      self$meta$HORIZONTAL_INTERVAL <- self$rd("float", 176, 4)
      self$meta$HORIZONTAL_OFFSET <- self$rd("double", 180, 8)
      self$meta$PIXEL_OFFSET <- self$rd("double", 188, 8)
      self$meta$VERTUNIT <- self$rd("string", 196, 48)
      self$meta$HORUNIT <- self$rd("string", 244, 48)
      self$meta$HORIZ_UNCERTAINTY <- self$rd("float", 292, 4)
      self$meta$TRIGGER_TIME <- self$get_timestamp(296, 16)
      self$meta$ACQ_DURATION <- self$rd("float", 312, 4)
      self$meta$CA_RECORD_TYPE <- self$record_type[1 + self$rd("short", 316, 2)]
      self$meta$PROCESSING_DONE <- self$processing_done[1 + self$rd("short", 318, 2)]
      self$meta$RIS_SWEEPS <- self$rd("short", 322, 2)
      self$meta$TIME_BASE <- self$timebase[1 + self$rd("short", 324, 2)]
      self$meta$VERTICAL_COUPLING <- self$vert_coupling[1 + self$rd("short", 326, 2)]
      self$meta$PROBE_ATTENUATION <- self$rd("float", 328, 4)
      self$meta$FIXED_VERTICAL_GAIN <- self$fixed_vert_gain[1 + self$rd("short", 332, 2)]
      # self$meta$BAND_WIDTH_LIMIT <- as.logical(self$rd("short", 334, 2))
      self$meta$BAND_WIDTH_LIMIT <- self$bandwidth_limit[1 + self$rd("short", 334, 2)]
      self$meta$VERTICAL_VERNIER <- self$rd("float", 336, 4)
      self$meta$ACQ_VERTICAL_OFFSET <- self$rd("float", 340, 4)
      self$meta$WAVE_SOURCE <- self$wave_source[1 + self$rd("short", 344, 2)]

      xlab <- str_c("x(", self$meta$HORUNIT, ")")
      ylab <- str_c("y(", self$meta$VERTUNIT, ")")

      y <- self$meta$VERTICAL_GAIN * self$get_y() - self$meta$VERTICAL_OFFSET
      x <- (0:(length(y) - 1)) * self$meta$HORIZONTAL_INTERVAL + self$meta$HORIZONTAL_OFFSET
      self$wfm <- tibble(
        !!xlab := x,
        !!ylab := y
      )
    },
    to_tsv = function(tsv_file = NULL) {
      if (is.null(tsv_file)) tsv_file <- str_c(self$trc_file, ".tsv")
      write_tsv(self$wfm, tsv_file)
      message("save to: ", tsv_file)
    },
    plot = function(downsample = 50) {
      if (self$meta$HORUNIT == "S") {
        s0 <- str_c(sprintf(1 / self$meta$HORIZONTAL_INTERVAL * 1e-9, fmt = "%#.1f"), "GS/s")
      } else if (self$meta$HORUNIT == "HZ") {
        s0 <- str_c("df = ", sprintf(self$meta$HORIZONTAL_INTERVAL * 1e-6, fmt = "%#.5f"), "MHz")
      } else {
        s0 <- ""
      }

      s1 <- ifelse(downsample == 1, "", str_c(", downsample ", downsample, "x"))

      xlab <- names(self$wfm)[1]
      ylab <- names(self$wfm)[2]

      p <- ggplot(self$wfm[seq(1, nrow(self$wfm), downsample), ], aes(x = .data[[xlab]], y = .data[[ylab]])) +
        geom_line() +
        geom_point() +
        theme_bw() +
        labs(title = str_c(s0, s1))
      print(ggplotly(p))
    }
  )
)

trc <- Trc$new()
trc$decode("01_data/F1--Trace--00000.trc")
trc$print()
trc$plot(downsample = 1)
# trc$to_tsv()
